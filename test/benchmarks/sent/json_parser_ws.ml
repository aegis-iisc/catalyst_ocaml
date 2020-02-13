exception JSONTestEx of string
exception ParseKeyEx

type json = 
			Empty
			| Num of int 
			| Str of int 
			| Obj of kv_pair 


and kv_pair = Pair of int * json 
		

(*the return type for a parser (ParsedValue, Remaining List)*)
type output  = ParseRes of (json) * (int list)
type twolists = LPair of (json list) * (int list)



let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                        temp2 
(*proj for Pair (json, int list)*)
let projl lp = 
	match lp with 
		| ParseRes (l, r) -> l
	
let projr lp = 
	match  lp with
	| ParseRes (l, r) -> r


(*proj for LPair (int list, int list)*)
(* let lprojl lp = 
	match lp with 
		| LPair (l, r) -> l
	
let lprojr lp = 
	match  lp with
	| LPair (l, r) -> r

 *)
(* let rec string_of_intlist ls = 
    match ls with 
      []-> "[]"
      | x :: xs -> "[ "^(string_of_int x)^" : "^(string_of_intlist xs)^" ]" 
 *)
(*matches a tab or a space ascii value*)
let parse_space inp = 
	if ((inp = 32) || (inp = 9) ) then 
		true  else 
		false

(*trims the white spaces using many1 ws/tab*)

let rec parse_spaces inp =
	match inp with 
	[] -> []
	| x :: xs -> 
		let one = parse_space x in 
		if(one) then 
			let trimmed = parse_spaces xs in 
			trimmed  
		else
			inp 	


(*token : (ls -> {p_out, rem}) -> {inp  | relation (input is ls with some possible ws)} ->  (p_out, rem)*)
let token p inp= 
	let ls = parse_spaces inp in 
	let res = p ls in 
	res



let parse_str str = 
	match str with
	[] -> ParseRes (Empty, [])
	| x :: xs -> ParseRes (Str x, xs) 


let parse_num num = 
	match num with
	[] -> ParseRes (Empty, [])
	| x :: xs -> ParseRes (Num x, xs) 



let lookahead_colon ls =
	let ls_trimmed = parse_spaces ls in  
	match ls_trimmed with 
	[] -> false
	| x :: xs -> if (x = 58) then true else false


(* 

 let rec p_star acc inpc p = 
	match inpc with 
		[] ->
			let em = [] in  
			LPair (acc, em)
		| x :: xs ->  
					let em = [] in
					let chunk_i = token p inpc in 
					let p1 = projl (chunk_i) in
					let rem1 = projr (chunk_i) in 
					let acc' = concat acc p1 in 
					match rem1 with
						[] ->  LPair (acc', em)
						| x1 :: xs1 -> p_star acc' rem1 p 
 *)

(*a simple version of key, a key is only a single char*)
let parse_key str_trimmed = 
	if (str_trimmed >= 97 && str_trimmed <= 122) then 
		str_trimmed 
	else
		raise ParseKeyEx

(* let parse_char c = 
	if (c >= 97 && c <= 122) then 
		c 
	else
		raise ParseKeyEx

let rec string_for_int_list  ls = 
	match ls with 
		[] -> ""
		| x :: xs -> ((string_of_int x)^(string_for_list xs)) 

let parse_key str = 
	let res = p_star [] str parse_char in
	let k_list = lprojl res in 
	let rem = lprojr res in 
 	let key = int_of_string(string_for_int_list k_list) in 
 	ParseRes (key, rem)

 *)

	
let skip_colon colon_str = 
	let colon_str_trimmed = parse_spaces colon_str in 
	match colon_str_trimmed with 
	[] -> raise (JSONTestEx "nothing to skip")
	| x :: xs -> if (x = 58) then xs else raise (JSONTestEx ("trying to skip 58 while have "^(string_of_int x)))
 

let rec parse_kvpair input = 
	match input with 
	[] -> ParseRes(Empty, [])
	| x :: xs ->  
			let colon = lookahead_colon xs in 
			if (colon = true) then 
				let key = parse_key x in 
				let value_rem = token parse_value xs in 
				let value = projl value_rem in 
				let rem = projr value_rem in 
				let json = Obj (Pair (key, value)) in
				let res = ParseRes (json, rem) in 
				res
			else 
				raise (JSONTestEx "Not a key-value pair")


and parse_value inp =
	let no_colon_inp = skip_colon inp in 
	match no_colon_inp with 
		[] -> raise (JSONTestEx "empty value")
		| x :: xs -> 
			(*if x is "{" then the value is Obj*)
			if (x = 123) then 
				let obj_res = token parse_json inp in 
				let obj_value = projl obj_res in 
				let rem = projr obj_res in 
				ParseRes (obj_value , rem)
			else 
				if (x >= 97 && x <= 122) then 
					ParseRes (Str x, xs)
				else 
					ParseRes (Num x, xs)






(* 
parse_json : int list (list of ASCII values) ->  output
 *)
and parse_json ls = 
 	match ls with 
	[] -> ParseRes (Empty, []) 
	| x :: xs ->
		(*either parse a string, or parse a number *) 
		if (x = 123) (*case {*) then 
			let  kvpair = token parse_kvpair xs in 
		 	let rem = projr kvpair in 
		 	(match rem with (*test for terminal brace*)
		 		[] -> raise (JSONTestEx "empty remiander")
		 		| y :: ys -> 
		 			if (y = 125) then 
		 				kvpair
		 			else 

		 				raise (JSONTestEx ("no termianl }"^(string_of_int y))) )
		 			 		
		else
			if (x >= 97 && x <= 122) then (*test for characters*)
				let strjson = token parse_str ls in 
				strjson 
			else 
				if (x >= 48 && x <= 57) then  (*test for integers*)
					let numjson = token parse_num ls in 
					numjson	  
				else 	
					raise (JSONTestEx "no json case") 	



let () = 
	(* {a : 0}*)
	let input = [32;32;32;123; 97 ; 58 ;49;125] in 
	let _ = token parse_json input in 
	()
	