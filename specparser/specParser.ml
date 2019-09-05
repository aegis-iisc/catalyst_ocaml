
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNION
    | UINST
    | TRUE
    | SUBSETEQ
    | SUBSET
    | STAR
    | SEMICOLON
    | RPAREN
    | RELATION
    | RCURLY
    | RBRACE
    | PRIMITIVE
    | PLUS
    | PIPE
    | NOT
    | MINUS
    | LPAREN
    | LCURLY
    | LBRACE
    | LAMBDA
    | INT of (
# 54 "specParser.mly"
       (int)
# 31 "specParser.ml"
  )
    | IMPL
    | IFF
    | ID of (
# 53 "specParser.mly"
        (string)
# 38 "specParser.ml"
  )
    | FALSE
    | EQUALOP
    | EOL
    | EOF
    | DOT
    | DISJ
    | CROSSPRD
    | CONJ
    | COMMA
    | COLON
    | ASSUME
    | ARROW
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState156
  | MenhirState154
  | MenhirState152
  | MenhirState149
  | MenhirState144
  | MenhirState139
  | MenhirState134
  | MenhirState125
  | MenhirState123
  | MenhirState121
  | MenhirState117
  | MenhirState112
  | MenhirState110
  | MenhirState108
  | MenhirState96
  | MenhirState95
  | MenhirState93
  | MenhirState89
  | MenhirState88
  | MenhirState83
  | MenhirState81
  | MenhirState77
  | MenhirState74
  | MenhirState67
  | MenhirState66
  | MenhirState64
  | MenhirState59
  | MenhirState57
  | MenhirState53
  | MenhirState51
  | MenhirState45
  | MenhirState39
  | MenhirState37
  | MenhirState36
  | MenhirState34
  | MenhirState23
  | MenhirState21
  | MenhirState20
  | MenhirState12
  | MenhirState10
  | MenhirState7
  | MenhirState4
  | MenhirState3
  | MenhirState0

# 1 "specParser.mly"
  
open SpecLang
open RelLang
open Printf
module TypeSpec = SpecLang.RelSpec.TypeSpec
module RefTy = SpecLang.RefinementType
let defaultCons = SpecLang.Con.default
let symbase = "sp_"
let count = ref 0
let genVar = fun _ -> 
  let id = symbase ^ (string_of_int (!count)) in 
  let () = count := !count + 1
    in
      Var.fromString id 
let ($) (f,arg) = f arg
let  empty = fun _ -> Vector.new0 ()
let print msg = let () = Printf.printf "%s" msg in ()


# 133 "specParser.ml"

let rec _menhir_goto_patmatchseq : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_patmatchseq -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv623 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 143 "specParser.ml"
        )) * _menhir_state * 'tv_params)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_patmatchseq) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv621 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 151 "specParser.ml"
        )) * _menhir_state * 'tv_params)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((patmseq : 'tv_patmatchseq) : 'tv_patmatchseq) = _v in
        ((let (((_menhir_stack, _menhir_s), (i : (
# 53 "specParser.mly"
        (string)
# 158 "specParser.ml"
        ))), _, (p : 'tv_params)) = _menhir_stack in
        let _5 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_reldec = 
# 113 "specParser.mly"
          (StructuralRelation.T {id=RelId.fromString i;
                params = p;
                mapp = patmseq})
# 168 "specParser.ml"
         in
        _menhir_goto_reldec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv622)) : 'freshtv624)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv627 * _menhir_state * 'tv_patmatch)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_patmatchseq) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv625 * _menhir_state * 'tv_patmatch)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((pms : 'tv_patmatchseq) : 'tv_patmatchseq) = _v in
        ((let (_menhir_stack, _menhir_s, (pm : 'tv_patmatch)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_patmatchseq = 
# 133 "specParser.mly"
                                               (pm :: pms)
# 185 "specParser.ml"
         in
        _menhir_goto_patmatchseq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv626)) : 'freshtv628)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv631 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 193 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_patmatchseq) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv629 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 201 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((patmseq : 'tv_patmatchseq) : 'tv_patmatchseq) = _v in
        ((let ((_menhir_stack, _menhir_s), (i : (
# 53 "specParser.mly"
        (string)
# 208 "specParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_reldec = 
# 109 "specParser.mly"
          (StructuralRelation.T {id=RelId.fromString i;
                params = empty ();
                mapp = patmseq})
# 216 "specParser.ml"
         in
        _menhir_goto_reldec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv630)) : 'freshtv632)
    | _ ->
        _menhir_fail ()

and _menhir_goto_decsandtys : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_decsandtys -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv597 * _menhir_state * 'tv_primdec)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_decsandtys) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv595 * _menhir_state * 'tv_primdec)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((d : 'tv_decsandtys) : 'tv_decsandtys) = _v in
        ((let (_menhir_stack, _menhir_s, (p : 'tv_primdec)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_decsandtys = 
# 80 "specParser.mly"
                (match d with 
                  RelSpec.T ({reldecs; primdecs; 
                  typespecs}) -> 
                    RelSpec.T {primdecs = p :: primdecs; 
                              reldecs=reldecs; 
                              typespecs = typespecs}
                )
# 245 "specParser.ml"
         in
        _menhir_goto_decsandtys _menhir_env _menhir_stack _menhir_s _v) : 'freshtv596)) : 'freshtv598)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv601 * _menhir_state * 'tv_reldec)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_decsandtys) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv599 * _menhir_state * 'tv_reldec)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((d : 'tv_decsandtys) : 'tv_decsandtys) = _v in
        ((let (_menhir_stack, _menhir_s, (r : 'tv_reldec)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_decsandtys = 
# 72 "specParser.mly"
                  (
                    match d with 
                    RelSpec.T ({reldecs; primdecs; typespecs}) -> 
                    RelSpec.T {reldecs = r ::reldecs; 
                              primdecs = primdecs;
                            typespecs = typespecs}
                          )
# 268 "specParser.ml"
         in
        _menhir_goto_decsandtys _menhir_env _menhir_stack _menhir_s _v) : 'freshtv600)) : 'freshtv602)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv605 * _menhir_state * 'tv_typespec)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_decsandtys) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv603 * _menhir_state * 'tv_typespec)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((d : 'tv_decsandtys) : 'tv_decsandtys) = _v in
        ((let (_menhir_stack, _menhir_s, (t : 'tv_typespec)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_decsandtys = 
# 89 "specParser.mly"
                (
                  match d with
                 RelSpec.T {reldecs; primdecs; 
                  typespecs} -> 
                    RelSpec.T {reldecs = reldecs; primdecs=primdecs;
                      typespecs = t :: typespecs}
                )
# 291 "specParser.ml"
         in
        _menhir_goto_decsandtys _menhir_env _menhir_stack _menhir_s _v) : 'freshtv604)) : 'freshtv606)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv619) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_decsandtys) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv617) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((d : 'tv_decsandtys) : 'tv_decsandtys) = _v in
        ((let _v : 'tv_spec = 
# 67 "specParser.mly"
                   (
                  d)
# 307 "specParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv615) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_spec) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv613 * _menhir_state * 'tv_spec) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv609 * _menhir_state * 'tv_spec) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv607 * _menhir_state * 'tv_spec) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (s : 'tv_spec)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 60 "specParser.mly"
       (SpecLang.RelSpec.t)
# 329 "specParser.ml"
            ) = 
# 63 "specParser.mly"
               (s)
# 333 "specParser.ml"
             in
            _menhir_goto_start _menhir_env _menhir_stack _menhir_s _v) : 'freshtv608)) : 'freshtv610)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv611 * _menhir_state * 'tv_spec) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv612)) : 'freshtv614)) : 'freshtv616)) : 'freshtv618)) : 'freshtv620)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typespec : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typespec -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv593 * _menhir_state * 'tv_typespec) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589 * _menhir_state * 'tv_typespec) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSUME ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | LPAREN ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | PRIMITIVE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | RELATION ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | EOF ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152) : 'freshtv590)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv591 * _menhir_state * 'tv_typespec) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv592)) : 'freshtv594)

and _menhir_goto_vartyseq : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_vartyseq -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv583 * _menhir_state) * _menhir_state * 'tv_vartyseq) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv579 * _menhir_state) * _menhir_state * 'tv_vartyseq) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ARROW ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv573 * _menhir_state) * _menhir_state * 'tv_vartyseq)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (vas : 'tv_vartyseq)) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_vartyatom = 
# 217 "specParser.mly"
                                         (
                      
                      match vas with
                          [x] -> x 
                        | _ -> (genVar (), RefTy.Tuple 
                            (Vector.fromList vas))
                  )
# 415 "specParser.ml"
                 in
                _menhir_goto_vartyatom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv574)
            | COMMA | RPAREN | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv575 * _menhir_state) * _menhir_state * 'tv_vartyseq)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (vas : 'tv_vartyseq)) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_reftyatom = 
# 203 "specParser.mly"
                                        (
                         
                          match vas with
                                 
                          [(v, (RefTy.Base (_, _, _) as refty))] -> 
                              RefTy.alphaRenameToVar (refty) v
                        | [(v,refty)] -> refty
                        | _ -> RefTy.Tuple (Vector.fromList vas))
# 434 "specParser.ml"
                 in
                _menhir_goto_reftyatom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv576)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv577 * _menhir_state) * _menhir_state * 'tv_vartyseq)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv578)) : 'freshtv580)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv581 * _menhir_state) * _menhir_state * 'tv_vartyseq) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv582)) : 'freshtv584)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv587 * _menhir_state * 'tv_varty)) * _menhir_state * 'tv_vartyseq) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv585 * _menhir_state * 'tv_varty)) * _menhir_state * 'tv_vartyseq) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (vt : 'tv_varty)), _, (vts : 'tv_vartyseq)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_vartyseq = 
# 227 "specParser.mly"
                                       (vt :: vts)
# 461 "specParser.ml"
         in
        _menhir_goto_vartyseq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv586)) : 'freshtv588)
    | _ ->
        _menhir_fail ()

and _menhir_goto_rpatom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_rpatom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv571) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_rpatom) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv569) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((ra : 'tv_rpatom) : 'tv_rpatom) = _v in
    ((let _v : 'tv_patom = 
# 254 "specParser.mly"
                  (Predicate.Rel ra)
# 480 "specParser.ml"
     in
    _menhir_goto_patom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv570)) : 'freshtv572)

and _menhir_run108 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_rexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | LCURLY ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_rexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | LCURLY ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run112 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_rexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | LCURLY ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_goto_primdef : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_primdef -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv555 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 540 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_primdef) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv553 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 548 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((p : 'tv_primdef) : 'tv_primdef) = _v in
        ((let ((_menhir_stack, _menhir_s), (i : (
# 53 "specParser.mly"
        (string)
# 555 "specParser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_primdef = 
# 105 "specParser.mly"
                                    (PrimitiveRelation.Nary
                (Var.fromString i, p))
# 563 "specParser.ml"
         in
        _menhir_goto_primdef _menhir_env _menhir_stack _menhir_s _v) : 'freshtv554)) : 'freshtv556)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv567 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 571 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_primdef) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv565 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 579 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((p : 'tv_primdef) : 'tv_primdef) = _v in
        ((let ((_menhir_stack, _menhir_s), (i : (
# 53 "specParser.mly"
        (string)
# 586 "specParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_primdec = 
# 100 "specParser.mly"
                                                    (PrimitiveRelation.T
                    {id=RelId.fromString i; 
                    def=PrimitiveRelation.alphaRename p})
# 596 "specParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv563) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_primdec) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv561 * _menhir_state * 'tv_primdec) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv557 * _menhir_state * 'tv_primdec) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSUME ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
            | LPAREN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | PRIMITIVE ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | RELATION ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | EOF ->
                _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156) : 'freshtv558)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv559 * _menhir_state * 'tv_primdec) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv560)) : 'freshtv562)) : 'freshtv564)) : 'freshtv566)) : 'freshtv568)
    | _ ->
        _menhir_fail ()

and _menhir_goto_patmatch : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_patmatch -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv551 * _menhir_state * 'tv_patmatch) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PIPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv545 * _menhir_state * 'tv_patmatch) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv546)
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv547 * _menhir_state * 'tv_patmatch) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (pm : 'tv_patmatch)) = _menhir_stack in
        let _v : 'tv_patmatchseq = 
# 134 "specParser.mly"
                          ([pm])
# 669 "specParser.ml"
         in
        _menhir_goto_patmatchseq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv549 * _menhir_state * 'tv_patmatch) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv550)) : 'freshtv552)

and _menhir_run43 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_rexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv543 * _menhir_state) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
    ((let ((_menhir_stack, _menhir_s), _, (re : 'tv_rexpr)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : 'tv_ratom = 
# 171 "specParser.mly"
                               (re)
# 691 "specParser.ml"
     in
    _menhir_goto_ratom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv544)

and _menhir_goto_elemseq : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_elemseq -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv537 * _menhir_state)) * _menhir_state * 'tv_elemseq) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv533 * _menhir_state)) * _menhir_state * 'tv_elemseq) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RCURLY ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv529 * _menhir_state)) * _menhir_state * 'tv_elemseq)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv527 * _menhir_state)) * _menhir_state * 'tv_elemseq)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (els : 'tv_elemseq)) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_ratom = 
# 169 "specParser.mly"
                                                (T(Vector.fromList els))
# 725 "specParser.ml"
                 in
                _menhir_goto_ratom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv528)) : 'freshtv530)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv531 * _menhir_state)) * _menhir_state * 'tv_elemseq)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv532)) : 'freshtv534)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv535 * _menhir_state)) * _menhir_state * 'tv_elemseq) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv536)) : 'freshtv538)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv541 * _menhir_state * 'tv_elem)) * _menhir_state * 'tv_elemseq) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv539 * _menhir_state * 'tv_elem)) * _menhir_state * 'tv_elemseq) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (el : 'tv_elem)), _, (els : 'tv_elemseq)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_elemseq = 
# 174 "specParser.mly"
                                    (el::els)
# 752 "specParser.ml"
         in
        _menhir_goto_elemseq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv540)) : 'freshtv542)
    | _ ->
        _menhir_fail ()

and _menhir_reduce14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_decsandtys = 
# 96 "specParser.mly"
                (RelSpec.T {reldecs = [];
                  primdecs = Vector.new0 ();
                  typespecs = []})
# 765 "specParser.ml"
     in
    _menhir_goto_decsandtys _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_pred : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_pred -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * _menhir_state) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv497 * _menhir_state) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv495 * _menhir_state) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (pr : 'tv_pred)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_patom = 
# 253 "specParser.mly"
                              (pr)
# 791 "specParser.ml"
             in
            _menhir_goto_patom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv496)) : 'freshtv498)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv499 * _menhir_state) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv500)) : 'freshtv502)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv505 * _menhir_state * 'tv_patom)) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv503 * _menhir_state * 'tv_patom)) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (pa : 'tv_patom)), _, (pr : 'tv_pred)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_pred = 
# 246 "specParser.mly"
                              (Predicate.If (pa,pr))
# 811 "specParser.ml"
         in
        _menhir_goto_pred _menhir_env _menhir_stack _menhir_s _v) : 'freshtv504)) : 'freshtv506)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv509 * _menhir_state * 'tv_patom)) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv507 * _menhir_state * 'tv_patom)) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (pa : 'tv_patom)), _, (pr : 'tv_pred)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_pred = 
# 247 "specParser.mly"
                             (Predicate.Iff (pa,pr))
# 824 "specParser.ml"
         in
        _menhir_goto_pred _menhir_env _menhir_stack _menhir_s _v) : 'freshtv508)) : 'freshtv510)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv513 * _menhir_state * 'tv_patom)) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv511 * _menhir_state * 'tv_patom)) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (pa : 'tv_patom)), _, (pr : 'tv_pred)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_pred = 
# 249 "specParser.mly"
                              (Predicate.Disj (pa,pr))
# 837 "specParser.ml"
         in
        _menhir_goto_pred _menhir_env _menhir_stack _menhir_s _v) : 'freshtv512)) : 'freshtv514)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv517 * _menhir_state * 'tv_patom)) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv515 * _menhir_state * 'tv_patom)) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (pa : 'tv_patom)), _, (pr : 'tv_pred)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_pred = 
# 248 "specParser.mly"
                              (Predicate.Conj (pa,pr))
# 850 "specParser.ml"
         in
        _menhir_goto_pred _menhir_env _menhir_stack _menhir_s _v) : 'freshtv516)) : 'freshtv518)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv525 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 858 "specParser.ml"
        ))) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RCURLY ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv521 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 868 "specParser.ml"
            ))) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv519 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 875 "specParser.ml"
            ))) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (i : (
# 53 "specParser.mly"
        (string)
# 880 "specParser.ml"
            ))), _, (pr : 'tv_pred)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_basety = 
# 242 "specParser.mly"
                                         (RefinementType.Base ((Var.fromString i), 
                TyD.makeTunknown (), pr))
# 889 "specParser.ml"
             in
            _menhir_goto_basety _menhir_env _menhir_stack _menhir_s _v) : 'freshtv520)) : 'freshtv522)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv523 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 899 "specParser.ml"
            ))) * _menhir_state * 'tv_pred) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)) : 'freshtv526)
    | _ ->
        _menhir_fail ()

and _menhir_goto_refty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_refty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv467 * _menhir_state * 'tv_vartyatom)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_refty) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv465 * _menhir_state * 'tv_vartyatom)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((rt : 'tv_refty) : 'tv_refty) = _v in
        ((let (_menhir_stack, _menhir_s, (vrta : 'tv_vartyatom)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_refty = 
# 199 "specParser.mly"
                                      (  
                                          RefTy.Arrow ((Var.noName , (snd vrta)), rt))
# 924 "specParser.ml"
         in
        _menhir_goto_refty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv466)) : 'freshtv468)
    | MenhirState89 | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv481) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_refty) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((rt : 'tv_refty) : 'tv_refty) = _v in
        ((let _v : 'tv_varty = 
# 229 "specParser.mly"
                 (let open RefTy in 
                        match rt with
                          Base (v,_,_) -> (v,alphaRename rt)
                        | Tuple _ -> (genVar (),rt)
                        | Arrow _ -> (genVar (),rt)
              )
# 944 "specParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv477) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_varty) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv475 * _menhir_state * 'tv_varty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv469 * _menhir_state * 'tv_varty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
            | LCURLY ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | LPAREN ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv470)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv471 * _menhir_state * 'tv_varty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (vt : 'tv_varty)) = _menhir_stack in
            let _v : 'tv_vartyseq = 
# 226 "specParser.mly"
                    ([vt])
# 979 "specParser.ml"
             in
            _menhir_goto_vartyseq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv472)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv473 * _menhir_state * 'tv_varty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv474)) : 'freshtv476)) : 'freshtv478)) : 'freshtv480)) : 'freshtv482)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv485 * _menhir_state) * _menhir_state * 'tv_paramseq)) * (
# 53 "specParser.mly"
        (string)
# 994 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_refty) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv483 * _menhir_state) * _menhir_state * 'tv_paramseq)) * (
# 53 "specParser.mly"
        (string)
# 1002 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((rt : 'tv_refty) : 'tv_refty) = _v in
        ((let (((_menhir_stack, _menhir_s), _, (ps : 'tv_paramseq)), (i : (
# 53 "specParser.mly"
        (string)
# 1009 "specParser.ml"
        ))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_typespec = 
# 192 "specParser.mly"
                                                         (
                                  TypeSpec.T {isAssume = false;
                                name = Var.fromString i;
                                params = Vector.fromList ps; 
                                refty = rt})
# 1021 "specParser.ml"
         in
        _menhir_goto_typespec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)) : 'freshtv486)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv489 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1029 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_refty) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv487 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1037 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((rt : 'tv_refty) : 'tv_refty) = _v in
        ((let (_menhir_stack, _menhir_s, (i : (
# 53 "specParser.mly"
        (string)
# 1044 "specParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_typespec = 
# 188 "specParser.mly"
                               (      TypeSpec.T {isAssume = false;
                                       name = (Var.fromString i);
                                       params = empty ();
                                       refty = rt})
# 1053 "specParser.ml"
         in
        _menhir_goto_typespec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv488)) : 'freshtv490)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv493 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 1061 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_refty) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv491 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 1069 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((rt : 'tv_refty) : 'tv_refty) = _v in
        ((let ((_menhir_stack, _menhir_s), (i : (
# 53 "specParser.mly"
        (string)
# 1076 "specParser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_typespec = 
# 183 "specParser.mly"
                                      (
                                          TypeSpec.T {isAssume = true;
                                              name = (Var.fromString i);
                                              params = empty ();
                                              refty = rt})
# 1087 "specParser.ml"
         in
        _menhir_goto_typespec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv492)) : 'freshtv494)
    | _ ->
        _menhir_fail ()

and _menhir_goto_idseq : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_idseq -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv455 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1102 "specParser.ml"
        ))) * _menhir_state * 'tv_idseq) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv453 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1108 "specParser.ml"
        ))) * _menhir_state * 'tv_idseq) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 53 "specParser.mly"
        (string)
# 1113 "specParser.ml"
        ))), _, (is : 'tv_idseq)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_idseq = 
# 149 "specParser.mly"
                            ((Var.fromString i)::is)
# 1119 "specParser.ml"
         in
        _menhir_goto_idseq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv454)) : 'freshtv456)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv463) * _menhir_state * 'tv_idseq) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv459) * _menhir_state * 'tv_idseq) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv457) * _menhir_state * 'tv_idseq) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (is : 'tv_idseq)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_conargs = 
# 146 "specParser.mly"
                                 (Vector.fromList is)
# 1140 "specParser.ml"
             in
            _menhir_goto_conargs _menhir_env _menhir_stack _v) : 'freshtv458)) : 'freshtv460)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv461) * _menhir_state * 'tv_idseq) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv462)) : 'freshtv464)
    | _ ->
        _menhir_fail ()

and _menhir_goto_rexpr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_rexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv407 * _menhir_state) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv405 * _menhir_state) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)) : 'freshtv408)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * _menhir_state * 'tv_ratom)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv409 * _menhir_state * 'tv_ratom)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (ra : 'tv_ratom)), _, (re : 'tv_rexpr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_rexpr = 
# 164 "specParser.mly"
                                (U(ra,re))
# 1182 "specParser.ml"
         in
        _menhir_goto_rexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv410)) : 'freshtv412)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv415 * _menhir_state * 'tv_ratom)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv413 * _menhir_state * 'tv_ratom)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (ra : 'tv_ratom)), _, (re : 'tv_rexpr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_rexpr = 
# 165 "specParser.mly"
                                (D(ra,re))
# 1195 "specParser.ml"
         in
        _menhir_goto_rexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv414)) : 'freshtv416)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv419 * _menhir_state * 'tv_ratom)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv417 * _menhir_state * 'tv_ratom)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (ra : 'tv_ratom)), _, (re : 'tv_rexpr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_rexpr = 
# 163 "specParser.mly"
                                   (X(ra,re))
# 1208 "specParser.ml"
         in
        _menhir_goto_rexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv418)) : 'freshtv420)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv423 * _menhir_state) * 'tv_conpat))) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv421 * _menhir_state) * 'tv_conpat))) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), (cp : 'tv_conpat)), _, (re : 'tv_rexpr)) = _menhir_stack in
        let _4 = () in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_patmatch = 
# 138 "specParser.mly"
              (match cp with (c,vlop) -> (c, vlop, Expr re))
# 1223 "specParser.ml"
         in
        _menhir_goto_patmatch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)) : 'freshtv424)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv427 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1231 "specParser.ml"
        ))) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv425 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1237 "specParser.ml"
        ))) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 53 "specParser.mly"
        (string)
# 1242 "specParser.ml"
        ))), _, (re : 'tv_rexpr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_patmatch = 
# 139 "specParser.mly"
                                 ((Con.fromString i, None, Expr re))
# 1248 "specParser.ml"
         in
        _menhir_goto_patmatch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)) : 'freshtv428)
    | MenhirState74 | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv431 * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv429 * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (re : 'tv_rexpr)) = _menhir_stack in
        let _v : 'tv_primdef = 
# 104 "specParser.mly"
                   (PrimitiveRelation.Nullary re)
# 1260 "specParser.ml"
         in
        _menhir_goto_primdef _menhir_env _menhir_stack _menhir_s _v) : 'freshtv430)) : 'freshtv432)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv435 * _menhir_state) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALOP ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SUBSET ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | SUBSETEQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv433 * _menhir_state) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)) : 'freshtv436)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv439 * _menhir_state * 'tv_rexpr)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv437 * _menhir_state * 'tv_rexpr)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (re1 : 'tv_rexpr)), _, (re2 : 'tv_rexpr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_rpatom = 
# 266 "specParser.mly"
                                      (Predicate.RelPredicate.SubEq(re1,re2))
# 1294 "specParser.ml"
         in
        _menhir_goto_rpatom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv438)) : 'freshtv440)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv443 * _menhir_state * 'tv_rexpr)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv441 * _menhir_state * 'tv_rexpr)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (re1 : 'tv_rexpr)), _, (re2 : 'tv_rexpr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_rpatom = 
# 265 "specParser.mly"
                                    (Predicate.RelPredicate.Sub(re1,re2))
# 1307 "specParser.ml"
         in
        _menhir_goto_rpatom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)) : 'freshtv444)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv447 * _menhir_state * 'tv_rexpr)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv445 * _menhir_state * 'tv_rexpr)) * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (re1 : 'tv_rexpr)), _, (re2 : 'tv_rexpr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_rpatom = 
# 264 "specParser.mly"
                                     (Predicate.RelPredicate.Eq(re1,re2))
# 1320 "specParser.ml"
         in
        _menhir_goto_rpatom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)) : 'freshtv448)
    | MenhirState93 | MenhirState95 | MenhirState125 | MenhirState123 | MenhirState121 | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451 * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALOP ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | SUBSET ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | SUBSETEQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv449 * _menhir_state * 'tv_rexpr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv450)) : 'freshtv452)
    | _ ->
        _menhir_fail ()

and _menhir_goto_elem : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_elem -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv403 * _menhir_state * 'tv_elem) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv397 * _menhir_state * 'tv_elem) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | ID _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | INT _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv398)
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv399 * _menhir_state * 'tv_elem) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (el : 'tv_elem)) = _menhir_stack in
        let _v : 'tv_elemseq = 
# 173 "specParser.mly"
                  ([el])
# 1378 "specParser.ml"
         in
        _menhir_goto_elemseq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv401 * _menhir_state * 'tv_elem) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)) : 'freshtv404)

and _menhir_goto_reldec : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_reldec -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv395 * _menhir_state * 'tv_reldec) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv391 * _menhir_state * 'tv_reldec) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSUME ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | LPAREN ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | PRIMITIVE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | RELATION ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | EOF ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv392)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv393 * _menhir_state * 'tv_reldec) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)) : 'freshtv396)

and _menhir_goto_instexprs : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_instexprs -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv385 * _menhir_state) * _menhir_state * 'tv_instexpr)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_instexprs) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv383 * _menhir_state) * _menhir_state * 'tv_instexpr)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((ies : 'tv_instexprs) : 'tv_instexprs) = _v in
        ((let ((_menhir_stack, _menhir_s), _, (ie : 'tv_instexpr)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_instexprs = 
# 160 "specParser.mly"
                                                    (ie :: ies)
# 1445 "specParser.ml"
         in
        _menhir_goto_instexprs _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)) : 'freshtv386)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv389 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1453 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_instexprs) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv387 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1461 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((ies : 'tv_instexprs) : 'tv_instexprs) = _v in
        ((let (_menhir_stack, _menhir_s, (i : (
# 53 "specParser.mly"
        (string)
# 1468 "specParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_instexpr = 
# 154 "specParser.mly"
                              (RInst {
                sargs = empty (); targs = empty();
                args = Vector.fromList ies;
                rel = RelId.fromString i})
# 1476 "specParser.ml"
         in
        _menhir_goto_instexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv388)) : 'freshtv390)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_patom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_patom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState93 | MenhirState125 | MenhirState123 | MenhirState121 | MenhirState117 | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv377 * _menhir_state * 'tv_patom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONJ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv365 * _menhir_state * 'tv_patom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | LBRACE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | LCURLY ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | LPAREN ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | NOT ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | TRUE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv366)
        | DISJ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv367 * _menhir_state * 'tv_patom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | LBRACE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LCURLY ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LPAREN ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | NOT ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | TRUE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv368)
        | IFF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv369 * _menhir_state * 'tv_patom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | LBRACE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LCURLY ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LPAREN ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | NOT ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | TRUE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv370)
        | IMPL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv371 * _menhir_state * 'tv_patom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | LBRACE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | LCURLY ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | LPAREN ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | NOT ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | TRUE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv372)
        | RCURLY | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv373 * _menhir_state * 'tv_patom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (pa : 'tv_patom)) = _menhir_stack in
            let _v : 'tv_pred = 
# 245 "specParser.mly"
                 (pa)
# 1592 "specParser.ml"
             in
            _menhir_goto_pred _menhir_env _menhir_stack _menhir_s _v) : 'freshtv374)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv375 * _menhir_state * 'tv_patom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv381 * _menhir_state) * _menhir_state * 'tv_patom) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv379 * _menhir_state) * _menhir_state * 'tv_patom) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (pa : 'tv_patom)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_patom = 
# 252 "specParser.mly"
                     (Predicate.Not pa)
# 1612 "specParser.ml"
         in
        _menhir_goto_patom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)) : 'freshtv382)
    | _ ->
        _menhir_fail ()

and _menhir_goto_bpatom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bpatom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv363) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_bpatom) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv361) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((ba : 'tv_bpatom) : 'tv_bpatom) = _v in
    ((let _v : 'tv_patom = 
# 255 "specParser.mly"
                  (Predicate.Base ba)
# 1631 "specParser.ml"
     in
    _menhir_goto_patom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv362)) : 'freshtv364)

and _menhir_goto_reftyatom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_reftyatom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv359) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_reftyatom) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv357) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((rta : 'tv_reftyatom) : 'tv_reftyatom) = _v in
    ((let _v : 'tv_refty = 
# 198 "specParser.mly"
                      ( rta)
# 1648 "specParser.ml"
     in
    _menhir_goto_refty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)) : 'freshtv360)

and _menhir_goto_vartyatom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_vartyatom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv355 * _menhir_state * 'tv_vartyatom) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARROW ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351 * _menhir_state * 'tv_vartyatom) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | LCURLY ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | LPAREN ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134) : 'freshtv352)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353 * _menhir_state * 'tv_vartyatom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv354)) : 'freshtv356)

and _menhir_goto_params : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_params -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv341 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1693 "specParser.ml"
        )) * _menhir_state * 'tv_params) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv339 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1699 "specParser.ml"
        )) * _menhir_state * 'tv_params) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 53 "specParser.mly"
        (string)
# 1704 "specParser.ml"
        ))), _, (p : 'tv_params)) = _menhir_stack in
        let _v : 'tv_params = 
# 128 "specParser.mly"
                       ((RelId.fromString i)::p)
# 1709 "specParser.ml"
         in
        _menhir_goto_params _menhir_env _menhir_stack _menhir_s _v) : 'freshtv340)) : 'freshtv342)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv349 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 1717 "specParser.ml"
        )) * _menhir_state * 'tv_params) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv345 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 1727 "specParser.ml"
            )) * _menhir_state * 'tv_params) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUALOP ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv343 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 1737 "specParser.ml"
                )) * _menhir_state * 'tv_params)) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState7 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ID _v ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv344)
            | ID _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv346)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv347 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 1765 "specParser.ml"
            )) * _menhir_state * 'tv_params) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)) : 'freshtv350)
    | _ ->
        _menhir_fail ()

and _menhir_goto_conpat : _menhir_env -> 'ttv_tail -> 'tv_conpat -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv337 * _menhir_state) * 'tv_conpat) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv333 * _menhir_state) * 'tv_conpat) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv329 * _menhir_state) * 'tv_conpat)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | LCURLY ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | LPAREN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv330)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv331 * _menhir_state) * 'tv_conpat)) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv335 * _menhir_state) * 'tv_conpat) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "specParser.mly"
        (string)
# 1820 "specParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1832 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv324)
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1848 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (i : (
# 53 "specParser.mly"
        (string)
# 1853 "specParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_idseq = 
# 148 "specParser.mly"
             ([Var.fromString i])
# 1858 "specParser.ml"
         in
        _menhir_goto_idseq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv327 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 1868 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)

and _menhir_goto_conargs : _menhir_env -> 'ttv_tail -> 'tv_conargs -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv321 * (
# 53 "specParser.mly"
        (string)
# 1879 "specParser.ml"
    )) = Obj.magic _menhir_stack in
    let (_v : 'tv_conargs) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv319 * (
# 53 "specParser.mly"
        (string)
# 1886 "specParser.ml"
    )) = Obj.magic _menhir_stack in
    let ((co : 'tv_conargs) : 'tv_conargs) = _v in
    ((let (_menhir_stack, (i : (
# 53 "specParser.mly"
        (string)
# 1892 "specParser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_conpat = 
# 143 "specParser.mly"
                          ((Con.fromString i, Some co))
# 1897 "specParser.ml"
     in
    _menhir_goto_conpat _menhir_env _menhir_stack _v) : 'freshtv320)) : 'freshtv322)

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv317) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let t = () in
    let _v : 'tv_elem = 
# 177 "specParser.mly"
              (Bool(true))
# 1911 "specParser.ml"
     in
    _menhir_goto_elem _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)

and _menhir_goto_ratom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ratom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv315 * _menhir_state * 'tv_ratom) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CROSSPRD ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_ratom) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | LCURLY ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LPAREN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv306)
    | MINUS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307 * _menhir_state * 'tv_ratom) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | LCURLY ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LPAREN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv308)
    | UNION ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv309 * _menhir_state * 'tv_ratom) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | LCURLY ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LPAREN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv310)
    | CONJ | DISJ | EQUALOP | IFF | IMPL | PIPE | RCURLY | RPAREN | SEMICOLON | SUBSET | SUBSETEQ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv311 * _menhir_state * 'tv_ratom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (ra : 'tv_ratom)) = _menhir_stack in
        let _v : 'tv_rexpr = 
# 166 "specParser.mly"
                 (ra)
# 1978 "specParser.ml"
         in
        _menhir_goto_rexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv313 * _menhir_state * 'tv_ratom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)) : 'freshtv316)

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 54 "specParser.mly"
       (int)
# 1992 "specParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv303) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((ii : (
# 54 "specParser.mly"
       (int)
# 2002 "specParser.ml"
    )) : (
# 54 "specParser.mly"
       (int)
# 2006 "specParser.ml"
    )) = _v in
    ((let _v : 'tv_elem = 
# 176 "specParser.mly"
              (Int(ii))
# 2011 "specParser.ml"
     in
    _menhir_goto_elem _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "specParser.mly"
        (string)
# 2018 "specParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv301) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 53 "specParser.mly"
        (string)
# 2028 "specParser.ml"
    )) : (
# 53 "specParser.mly"
        (string)
# 2032 "specParser.ml"
    )) = _v in
    ((let _v : 'tv_elem = 
# 179 "specParser.mly"
            (Var(Var.fromString i))
# 2037 "specParser.ml"
     in
    _menhir_goto_elem _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv299) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let f = () in
    let _v : 'tv_elem = 
# 178 "specParser.mly"
               (Bool(false))
# 2051 "specParser.ml"
     in
    _menhir_goto_elem _menhir_env _menhir_stack _menhir_s _v) : 'freshtv300)

and _menhir_goto_instexpr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_instexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv265 * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv261 * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | LPAREN | RBRACE | STAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv259 * _menhir_state) * _menhir_state * 'tv_instexpr)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (ie : 'tv_instexpr)) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_instexprs = 
# 159 "specParser.mly"
                                      ([ie])
# 2082 "specParser.ml"
                 in
                _menhir_goto_instexprs _menhir_env _menhir_stack _menhir_s _v) : 'freshtv260)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv262)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv263 * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)) : 'freshtv266)
    | MenhirState93 | MenhirState95 | MenhirState96 | MenhirState125 | MenhirState123 | MenhirState121 | MenhirState117 | MenhirState112 | MenhirState110 | MenhirState108 | MenhirState74 | MenhirState77 | MenhirState57 | MenhirState20 | MenhirState21 | MenhirState53 | MenhirState51 | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv277 * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv273 * _menhir_state * 'tv_instexpr)) = Obj.magic _menhir_stack in
                let (_v : (
# 53 "specParser.mly"
        (string)
# 2114 "specParser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | RPAREN ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv269 * _menhir_state * 'tv_instexpr)) * (
# 53 "specParser.mly"
        (string)
# 2125 "specParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_instexpr)) * (
# 53 "specParser.mly"
        (string)
# 2132 "specParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, (ie : 'tv_instexpr)), (i : (
# 53 "specParser.mly"
        (string)
# 2137 "specParser.ml"
                    ))) = _menhir_stack in
                    let _4 = () in
                    let _2 = () in
                    let _v : 'tv_ratom = 
# 170 "specParser.mly"
                                       (R (ie, Var.fromString i))
# 2144 "specParser.ml"
                     in
                    _menhir_goto_ratom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)) : 'freshtv270)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv271 * _menhir_state * 'tv_instexpr)) * (
# 53 "specParser.mly"
        (string)
# 2154 "specParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)) : 'freshtv274)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv275 * _menhir_state * 'tv_instexpr)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)) : 'freshtv278)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv279 * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)) : 'freshtv282)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv289 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 2177 "specParser.ml"
        )) * _menhir_state * 'tv_params)) * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv285 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 2187 "specParser.ml"
            )) * _menhir_state * 'tv_params)) * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv283 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 2194 "specParser.ml"
            )) * _menhir_state * 'tv_params)) * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), (i : (
# 53 "specParser.mly"
        (string)
# 2199 "specParser.ml"
            ))), _, (p : 'tv_params)), _), _, (ie : 'tv_instexpr)) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_reldec = 
# 122 "specParser.mly"
          (StructuralRelation.T{id=RelId.fromString i;
                params = p;
                mapp = [(defaultCons,None,
                  Star ie)]})
# 2212 "specParser.ml"
             in
            _menhir_goto_reldec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)) : 'freshtv286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv287 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 2222 "specParser.ml"
            )) * _menhir_state * 'tv_params)) * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv297 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2231 "specParser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv293 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2241 "specParser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv291 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2248 "specParser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (i : (
# 53 "specParser.mly"
        (string)
# 2253 "specParser.ml"
            ))), _), _, (ie : 'tv_instexpr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_reldec = 
# 117 "specParser.mly"
          (StructuralRelation.T{id=RelId.fromString i;
                params = empty ();
                mapp = [(defaultCons,None,
                  Star ie)]})
# 2264 "specParser.ml"
             in
            _menhir_goto_reldec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)) : 'freshtv294)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv295 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2274 "specParser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_instexpr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)) : 'freshtv298)
    | _ ->
        _menhir_fail ()

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_goto_paramseq : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_paramseq -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv243 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 2303 "specParser.ml"
        ))) * _menhir_state * 'tv_paramseq) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv241 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 2309 "specParser.ml"
        ))) * _menhir_state * 'tv_paramseq) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 53 "specParser.mly"
        (string)
# 2314 "specParser.ml"
        ))), _, (pseq : 'tv_paramseq)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_paramseq = 
# 131 "specParser.mly"
                                  ((RelId.fromString i)::pseq)
# 2320 "specParser.ml"
         in
        _menhir_goto_paramseq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)) : 'freshtv244)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * _menhir_state) * _menhir_state * 'tv_paramseq) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv253 * _menhir_state) * _menhir_state * 'tv_paramseq) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv249 * _menhir_state) * _menhir_state * 'tv_paramseq)) = Obj.magic _menhir_stack in
                let (_v : (
# 53 "specParser.mly"
        (string)
# 2341 "specParser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | COLON ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv245 * _menhir_state) * _menhir_state * 'tv_paramseq)) * (
# 53 "specParser.mly"
        (string)
# 2352 "specParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ID _v ->
                        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
                    | LCURLY ->
                        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                    | LPAREN ->
                        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv246)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv247 * _menhir_state) * _menhir_state * 'tv_paramseq)) * (
# 53 "specParser.mly"
        (string)
# 2374 "specParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv251 * _menhir_state) * _menhir_state * 'tv_paramseq)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)) : 'freshtv254)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv255 * _menhir_state) * _menhir_state * 'tv_paramseq) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)) : 'freshtv258)
    | _ ->
        _menhir_fail ()

and _menhir_run94 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv239) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_patom = 
# 251 "specParser.mly"
             (Predicate.truee())
# 2405 "specParser.ml"
     in
    _menhir_goto_patom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv240)

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | LBRACE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | LCURLY ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | LPAREN ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NOT ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | TRUE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | LBRACE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | LCURLY ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | LPAREN ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | NOT ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | TRUE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 53 "specParser.mly"
        (string)
# 2467 "specParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv231 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2478 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv211 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2488 "specParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | RBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv207 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2498 "specParser.ml"
                    )))) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv205 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2505 "specParser.ml"
                    )))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), (i1 : (
# 53 "specParser.mly"
        (string)
# 2510 "specParser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _4 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : 'tv_bpatom = 
# 261 "specParser.mly"
                                           (Predicate.BasePredicate.varBoolEq 
                      (Var.fromString i1, false))
# 2520 "specParser.ml"
                     in
                    _menhir_goto_bpatom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv206)) : 'freshtv208)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv209 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2530 "specParser.ml"
                    )))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)) : 'freshtv212)
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv219 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2539 "specParser.ml"
                ))) = Obj.magic _menhir_stack in
                let (_v : (
# 53 "specParser.mly"
        (string)
# 2544 "specParser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | RBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv215 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2555 "specParser.ml"
                    ))) * (
# 53 "specParser.mly"
        (string)
# 2559 "specParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv213 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2566 "specParser.ml"
                    ))) * (
# 53 "specParser.mly"
        (string)
# 2570 "specParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), (i1 : (
# 53 "specParser.mly"
        (string)
# 2575 "specParser.ml"
                    ))), (i2 : (
# 53 "specParser.mly"
        (string)
# 2579 "specParser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : 'tv_bpatom = 
# 257 "specParser.mly"
                                           (Predicate.BasePredicate.varEq 
                      (Var.fromString i1, Var.fromString i2))
# 2588 "specParser.ml"
                     in
                    _menhir_goto_bpatom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)) : 'freshtv216)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv217 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2598 "specParser.ml"
                    ))) * (
# 53 "specParser.mly"
        (string)
# 2602 "specParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)) : 'freshtv220)
            | TRUE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv227 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2611 "specParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | RBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv223 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2621 "specParser.ml"
                    )))) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv221 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2628 "specParser.ml"
                    )))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), (i1 : (
# 53 "specParser.mly"
        (string)
# 2633 "specParser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _4 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : 'tv_bpatom = 
# 259 "specParser.mly"
                                          (Predicate.BasePredicate.varBoolEq 
                      (Var.fromString i1, true))
# 2643 "specParser.ml"
                     in
                    _menhir_goto_bpatom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv222)) : 'freshtv224)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv225 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2653 "specParser.ml"
                    )))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv229 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2664 "specParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv233 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 2675 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)

and _menhir_goto_basety : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_basety -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv203 * _menhir_state * 'tv_basety) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARROW ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_basety) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (bt : 'tv_basety)) = _menhir_stack in
        let _v : 'tv_vartyatom = 
# 212 "specParser.mly"
                      (
                         
                      match bt with 
                      RefTy.Base (v,_,_) -> (v,bt)
                    | _ -> raise (Failure "Impossible case of basety"))
# 2706 "specParser.ml"
         in
        _menhir_goto_vartyatom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)
    | COMMA | RPAREN | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state * 'tv_basety) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (bt : 'tv_basety)) = _menhir_stack in
        let _v : 'tv_reftyatom = 
# 202 "specParser.mly"
                      ( bt)
# 2716 "specParser.ml"
         in
        _menhir_goto_reftyatom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv200)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201 * _menhir_state * 'tv_basety) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)) : 'freshtv204)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "specParser.mly"
        (string)
# 2730 "specParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 2744 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (i : (
# 53 "specParser.mly"
        (string)
# 2749 "specParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_params = 
# 127 "specParser.mly"
                ([RelId.fromString i])
# 2754 "specParser.ml"
         in
        _menhir_goto_params _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191) = Obj.magic _menhir_stack in
        let (_v : (
# 53 "specParser.mly"
        (string)
# 2774 "specParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv183) = Obj.magic _menhir_stack in
            let (_v : (
# 53 "specParser.mly"
        (string)
# 2786 "specParser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv181) = Obj.magic _menhir_stack in
            let ((i : (
# 53 "specParser.mly"
        (string)
# 2794 "specParser.ml"
            )) : (
# 53 "specParser.mly"
        (string)
# 2798 "specParser.ml"
            )) = _v in
            ((let _v : 'tv_conargs = 
# 145 "specParser.mly"
               (Vector.fromList [Var.fromString i])
# 2803 "specParser.ml"
             in
            _menhir_goto_conargs _menhir_env _menhir_stack _v) : 'freshtv182)) : 'freshtv184)
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv185) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv186)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv187 * (
# 53 "specParser.mly"
        (string)
# 2823 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, (i : (
# 53 "specParser.mly"
        (string)
# 2828 "specParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_conpat = 
# 142 "specParser.mly"
               ((Con.fromString i, None))
# 2833 "specParser.ml"
             in
            _menhir_goto_conpat _menhir_env _menhir_stack _v) : 'freshtv188)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv189 * (
# 53 "specParser.mly"
        (string)
# 2843 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv190)) : 'freshtv192)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "specParser.mly"
        (string)
# 2857 "specParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALOP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 2869 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | LCURLY ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LPAREN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv178)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 2891 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LCURLY ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | ID _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | INT _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv171 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState23 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RCURLY ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv167 * _menhir_state)) * _menhir_state) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv165 * _menhir_state)) * _menhir_state) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_ratom = 
# 168 "specParser.mly"
                                    (T(Vector.fromList []))
# 2953 "specParser.ml"
                 in
                _menhir_goto_ratom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)) : 'freshtv168)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv169 * _menhir_state)) * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)) : 'freshtv172)
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23) : 'freshtv174)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 53 "specParser.mly"
        (string)
# 2989 "specParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv157 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3000 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | LAMBDA ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LCURLY ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LPAREN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv158)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv159 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3024 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "specParser.mly"
        (string)
# 3039 "specParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LPAREN | RBRACE | STAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3053 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (i : (
# 53 "specParser.mly"
        (string)
# 3058 "specParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_instexpr = 
# 151 "specParser.mly"
                (RInst { sargs = empty (); 
                targs = empty(); args = empty (); 
                rel = RelId.fromString i})
# 3065 "specParser.ml"
         in
        _menhir_goto_instexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "specParser.mly"
        (string)
# 3076 "specParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3088 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv150)
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3104 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (i : (
# 53 "specParser.mly"
        (string)
# 3109 "specParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_paramseq = 
# 130 "specParser.mly"
                    ([RelId.fromString i])
# 3114 "specParser.ml"
         in
        _menhir_goto_paramseq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv152)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3124 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | LCURLY ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LPAREN ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 53 "specParser.mly"
        (string)
# 3158 "specParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PIPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv137 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3169 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | LBRACE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LCURLY ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LPAREN ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | NOT ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | TRUE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv138)
        | RCURLY ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv141 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3195 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv139 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3202 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), (i : (
# 53 "specParser.mly"
        (string)
# 3207 "specParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_basety = 
# 239 "specParser.mly"
                            (RefinementType.Base ((Var.fromString i), 
                TyD.makeTunknown (), 
                Predicate.truee()))
# 3216 "specParser.ml"
             in
            _menhir_goto_basety _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)) : 'freshtv142)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv143 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3226 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)) : 'freshtv146)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "specParser.mly"
        (string)
# 3241 "specParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 53 "specParser.mly"
        (string)
# 3251 "specParser.ml"
    )) : (
# 53 "specParser.mly"
        (string)
# 3255 "specParser.ml"
    )) = _v in
    ((let _v : 'tv_basety = 
# 236 "specParser.mly"
              (RefinementType.Base ((Var.fromString i), 
                TyD.makeTunknown (),
                Predicate.truee()))
# 3262 "specParser.ml"
     in
    _menhir_goto_basety _menhir_env _menhir_stack _menhir_s _v) : 'freshtv136)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * 'tv_primdec)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * 'tv_reldec)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_typespec)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv53 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3289 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3298 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_varty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_vartyatom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_patom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_patom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * 'tv_patom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_patom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_rexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_rexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_rexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv79 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3362 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv83 * _menhir_state) * _menhir_state * 'tv_paramseq)) * (
# 53 "specParser.mly"
        (string)
# 3376 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3385 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3399 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv91 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 3408 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv93 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3417 "specParser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3426 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state * 'tv_patmatch)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv99 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 3440 "specParser.ml"
        )) * _menhir_state * 'tv_params)) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3449 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state * 'tv_ratom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state * 'tv_ratom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state * 'tv_ratom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * _menhir_state) * _menhir_state * 'tv_instexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3483 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * _menhir_state * 'tv_elem)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv121 * _menhir_state) * 'tv_conpat))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3512 "specParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv126)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv127 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 3525 "specParser.ml"
        )) * _menhir_state * 'tv_params)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3534 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv131 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 3543 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv134)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 53 "specParser.mly"
        (string)
# 3564 "specParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv35 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3575 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState66 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv36)
        | ID _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv38)
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv39 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 53 "specParser.mly"
        (string)
# 3608 "specParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv41 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)) : 'freshtv44)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RELATION ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv27 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 53 "specParser.mly"
        (string)
# 3653 "specParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUALOP ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv23 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 3664 "specParser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ID _v ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
                | LAMBDA ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | LCURLY ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | LPAREN ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv24)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv25 * _menhir_state)) * (
# 53 "specParser.mly"
        (string)
# 3688 "specParser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)) : 'freshtv28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv29 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)) : 'freshtv32)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)

and _menhir_run81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run143 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "specParser.mly"
        (string)
# 3723 "specParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3735 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | LCURLY ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | LPAREN ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144) : 'freshtv20)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * (
# 53 "specParser.mly"
        (string)
# 3757 "specParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)

and _menhir_goto_start : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 60 "specParser.mly"
       (SpecLang.RelSpec.t)
# 3765 "specParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 60 "specParser.mly"
       (SpecLang.RelSpec.t)
# 3774 "specParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 60 "specParser.mly"
       (SpecLang.RelSpec.t)
# 3782 "specParser.ml"
    )) : (
# 60 "specParser.mly"
       (SpecLang.RelSpec.t)
# 3786 "specParser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv16)) : 'freshtv18)

and _menhir_run147 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 53 "specParser.mly"
        (string)
# 3802 "specParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv7 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3813 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | LCURLY ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | LPAREN ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149) : 'freshtv8)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv9 * _menhir_state) * (
# 53 "specParser.mly"
        (string)
# 3835 "specParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)) : 'freshtv12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and start : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 60 "specParser.mly"
       (SpecLang.RelSpec.t)
# 3862 "specParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSUME ->
        _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        ((let _1 = () in
        let _v : (
# 60 "specParser.mly"
       (SpecLang.RelSpec.t)
# 3894 "specParser.ml"
        ) = 
# 64 "specParser.mly"
              (RelSpec.mk_empty_relspec ())
# 3898 "specParser.ml"
         in
        _menhir_goto_start _menhir_env _menhir_stack _menhir_s _v) : 'freshtv2)) : 'freshtv4)
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LPAREN ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PRIMITIVE ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RELATION ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv6))

# 233 "/home/ashish/.opam/4.03.0/lib/menhir/standard.mly"
  

# 3917 "specParser.ml"
