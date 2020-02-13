#lang racket

(require racket/bytes)
(module+ test (require rackunit))

;; Read precisely len bytes without truncating on EOF. If len bytes
;; are not available, error.
(define (read-bytes* len port)
  (define value (read-bytes len port))
  (cond [(eof-object? value)
         (error "unexpected EOF")]
        [(not (= (bytes-length value) len))
         (error "not enough bytes available")]
        [else
         value]))

;; Read a mini-CBOR value from an input port, updating the position by side effect.
(define (read-mini-CBOR port)
  (define first-byte (read-byte port))
  (define major-type (bitwise-bit-field first-byte 5 8))
  (define additional-info (bitwise-bit-field first-byte 0 5))
  (match major-type
    [3
     (define byte-count
       (if (< additional-info 24)
           additional-info
           (read-byte port)))
     (bytes->string/utf-8 (read-bytes* byte-count port))]
    [4
     (define element-count
       (if (< additional-info 24)
           additional-info
           (read-byte port)))
     (build-vector element-count
                   (lambda (x)
                     (read-mini-CBOR port)))]
    [5
     (define element-count
       (if (< additional-info 24)
           additional-info
           (read-byte port)))
     (make-immutable-hash
      (build-list element-count
                  (lambda (x)
                    (cons
                     (read-mini-CBOR port)
                     (read-mini-CBOR port)))))]
    [7
     (if (not (= additional-info 26))
         (error (format "wanted 26, got ~a" additional-info))
         (floating-point-bytes->real (read-bytes* 4 port) #t))]))

;; Decode mini-CBOR from an explicit byte string value.
(define (decode-mini-CBOR bytestring)
  (with-input-from-bytes bytestring
    (lambda ()
      (read-mini-CBOR (current-input-port)))))

;;; Encoders for writing tests.
(define float-header (bitwise-ior (arithmetic-shift 7 5) 26))
(define (encode-float n)
  (define out (make-bytes 5))
  (bytes-set! out 0 float-header)
  (real->floating-point-bytes n 4 #t out 1))

(define (string-header len)
  (define byte-count (bytes-length len))
  (define first-byte
    (bitwise-ior (arithmetic-shift 3 5)
                 (cond [(<= byte-count 23)
                        byte-count]
                       [(> byte-count 255)
                        (error "string too long")]
                       [else 24])))
  (if (<= byte-count 23)
      (bytes first-byte)
      (bytes first-byte byte-count)))

(define (encode-string str)
  (define the-bytes (string->bytes/utf-8 str))
  (bytes-append (string-header the-bytes) the-bytes))

(define (array-header vec)
  (define entry-count (vector-length vec))
  (define first-byte
    (bitwise-ior (arithmetic-shift 4 5)
                 (cond [(<= entry-count 23)
                        entry-count]
                       [(> entry-count 255)
                        (error "array too long")]
                       [else 24])))
  (if (<= entry-count 23)
      (bytes first-byte)
      (bytes first-byte entry-count)))
(define (encode-vector vec)
  (define element-count (vector-length vec))
  (bytes-append* (array-header vec)
                 (for/list ([val (in-vector vec)])
                   (encode-mini-CBOR val))))

(define (hash-header vec)
  (define entry-count (hash-count vec))
  (define first-byte
    (bitwise-ior (arithmetic-shift 5 5)
                 (cond [(<= entry-count 23)
                        entry-count]
                       [(> entry-count 255)
                        (error "hash too big")]
                       [else 24])))
  (if (<= entry-count 23)
      (bytes first-byte)
      (bytes first-byte entry-count)))

(define (encode-hash h)
  (bytes-append* (hash-header h)
                 (for/list ([(k v) (in-hash h)])
                   (bytes-append (encode-mini-CBOR k) (encode-mini-CBOR v)))))

(define (encode-mini-CBOR val)
  (cond
    [(string? val) (encode-string val)]
    [(and (number? val) (inexact? val)) (encode-float val)]
    [(vector? val) (encode-vector val)]
    [(hash? val) (encode-hash val)]
    [else (error "unsupported value")]))

(module+ test
  ;; Fit a float into 32 bits, even if the reader made something more accurate
  (define (shortify-float f)
    (floating-point-bytes->real (real->floating-point-bytes f 4 #t) #t))



  (for ([float (in-list (map shortify-float '(0.1 0.5 3.1234 55.22985 -0.435 +nan.0 1.0 2000000)))])
    (check-eqv? float (decode-mini-CBOR (encode-float float))))

  (for ([str (in-list '("" "hello" "asldkfjals;dkj" "jeg er ude på øen i åen, er jeg" "🛶"))])
    (check-equal? str (decode-mini-CBOR (encode-string str))))

  (define tests
    (list "foo"
          (shortify-float 4.23)
          (vector)
          (vector (shortify-float 1.0) "cashew-based ice cream")
          (vector (shortify-float 1.0)
                  (shortify-float 2.3)
                  (hash "a" (shortify-float 44.0) "b" (shortify-float 34.4))
                  (vector))
          (hash (hash (shortify-float 1.2) (shortify-float 2.3)) 23.0
                "hello" (hash))))

  (for ([v (in-list tests)])
    (check-equal? v (decode-mini-CBOR (encode-mini-CBOR v)))))
