exception TestExp 




(*the return type for a parser (ParsedValue, Remaining List)*)
type listPair  = Pair of (int list) * (int list)

type bit = Zero | One 
type pair = BPair of bit * bit 
type list_bpair =
      E 
      | L of pair 
      | LCons of pair * list_bpair


let projl lp = 
	match lp with 
		| Pair (l, r) -> l
	
let projr lp = 
	match  lp with
	| Pair(l, r) -> r

(*Pervasive library functions, 
comment to typecheck, uncomment to excute the tests*)
let eq x1 y1 = x1 = y1
let minus_one n1 = n1 - 1
		
(* uncomment to typecheck*) 
(* let eq x1 y1 = true
let minus_one n1 = n1
let raise ex = 
		let em = [] in 
		let resex = Pair (em,em) in 
		resex
 *)
(*printing utilities, comment to typecheck uncomment to excute the test*)
let rec string_of_intlist ls = 
    match ls with 
      []-> "[]"
      | x :: xs -> "[ "^(string_of_int x)^" : "^(string_of_intlist xs)^" ]" 

let string_of_bin b = 
	match b with 
		| Zero -> "0"
		| One -> "1"

let rec string_of_binlist ls = 
    match ls with 
      []-> "[]"
      | x :: xs -> "[ "^(string_of_bin x)^" : "^(string_of_binlist xs)^" ]" 


let rec string_of_pair p = 
	match p with 
		Pair (l1, l2) -> "{ parsed "^(string_of_intlist l1)^", \n remaining "^(string_of_intlist l2)^" }" 


let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                        temp2 


let rec parse_chunk ac n l = 
  let z = 0 in 
  let n_eq_0  = eq n z in 
  match l with 
    [] -> 
      if (n_eq_0) then 
      		let em = [] in 
        	let res0 = Pair (ac,em) in 
        	res0  
      	else 
        	raise TestExp
  | x :: xs ->
      let n_min_1 = minus_one n in 
      if (n_eq_0) then 
      		let res1 = Pair (ac,l) in 
      		res1
      else 
      let em = [] in 	
      let t1 =  x :: em in 
      let ac' = concat ac t1 in 
      let res2 = parse_chunk ac' n_min_1 xs in
      res2

 
 

 let rec p_star acc inpc p = 
	match inpc with 
		[] ->
			let em = [] in  
			Pair (acc, em)
		| x :: xs ->  
					let em = [] in
					let chunk_i = p em  x xs in 
					let p1 = projl (chunk_i) in
					let rem1 = projr (chunk_i) in 
					let acc' = concat acc p1 in 
					
					match rem1 with
						[] ->  Pair (acc', em)
						| x1 :: xs1 -> p_star acc' rem1 p 

 let parse_header hd inp = 
	match inp with
	[] -> false 
	| x :: xs ->
		 let x_eq_hd = eq x hd in  
		 if (x_eq_hd) then 
		 	true 
		 else
		 	false


let parse_png hd inp = 
	match inp with 
		[] -> raise TestExp 
		| x :: xs -> 
					let h = parse_header hd inp in
					if (h) then  
						let em = [] in 
						let chunks = p_star em xs parse_chunk in 
						let res = projl chunks in 
						let rem = projr chunks in 
						Pair (res, rem)
					 else 
					 	raise TestExp  	


let xor_b p = 
  match p with 
  BPair (b1, b2) ->
    match b1 with 
      | Zero -> (match b2 with 
          Zero -> 0  
          | One -> 1)
      | One -> (match b2 with 
          Zero -> 1 
          | One -> 0)    




 
let rec map f ls = 
  match ls with 
    | E -> []
    | L p -> let f_p = f p in 
        let empty = [] in 
          let res = f_p :: empty in 
          res  
    | LCons (p, pl) ->
           let f_p = f p in
           let  f_pl = map f pl in    
           let res = f_p :: f_pl in 
                res

let xor pl = 
  let res = map xor_b pl in 
  res



let rec zip l1 l2 = 
  match l1 with 
   | [] -> (match l2 with 
           | [] -> E
           | x :: xs -> raise TestExp)
   | y :: ys -> (match l2 with 
           			| [] -> raise TestExp 
           			| x :: xs -> let result = zip ys xs in 
                        let p = BPair (y,x) in 
                        let res = LCons ( p , result) in 
                         res)

let generate_crc pl  =
  let res = xor pl in 
	res 


(*int_to_bin : i -> {b | }*)
let int_to_bin i = 
	if (i = 0 ) then Zero 
	else 
		One

let rec int_to_bin_list lint = 
	match lint with
	| [] -> []
	| x :: xs -> let bin_x = int_to_bin x in 
				 let bin_xs = int_to_bin_list xs in 
				 let bin_lint = bin_x :: bin_xs  in 
				 bin_lint

let full_png hd inp gen = 
	let png_wo_crc = parse_png hd inp in
	let data = projl png_wo_crc in
	let () = Printf.printf "%s" ("\ndata "^(string_of_intlist data)) in 
	let data_bin = int_to_bin_list data in   

	let data_and_gen = zip data_bin gen in 
	let crc = generate_crc data_and_gen in 
	let data_with_crc = concat data crc in 
	data_with_crc

 
 
(*@param src : list header, followed by a data, such that header gives the length of the remaining data*)
       
let ()  = 
	   let src = [88;3;0;1;0;4;1;1;0;0] in
	   let gen = [Zero;Zero;Zero;One;One;One;Zero] in  
      let parsedList  = full_png 88 src gen in 
      let () = Printf.printf "%s" ("\n test1 ") in 
      let () = Printf.printf "%s" ("\ninput list "^(string_of_intlist src)) in 
      let () = Printf.printf "%s" ("\nparsed list "^(string_of_intlist parsedList)) in 
     
        ()  
 
