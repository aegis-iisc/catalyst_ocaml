exception JSONTestEx of string
exception ParseKeyEx

type json = 
			Empty
			| Num of int 
			| Str of int 
			| Obj of kv_pair 


and kv_pair = Pair of int * json 
		

(*the return type for a parser (ParsedValue, Remaining List)*)
type json_rem  = ParseRes of (json) * (int list)

let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                        temp2 

let projl lp = 
	match lp with 
		| ParseRes (l, r) -> l
	
let projr lp = 
	match  lp with
	| ParseRes (l, r) -> r

(* type acc_res = AccRes of (json list) * (int list)
 *)
 (* let json_list acl = 
	match acl with 
		| AccRes (l, r) -> l
	
let rem_list acl = 
	match acl with 
		| AccRes (l, r) -> r

 *)
(*acc : json list *)
 (* let rec p_star acc inpc p = 
	match inpc with 
		[] ->
			let em = [] in  
			 AccRes (acc, em)
		| x :: xs ->  
					let em = [] in
					let json_i = p inpc in 
					let p1 = projl (json_i) in
					let rem1 = projr (json_i) in 
					let acc' = concat acc [p1] in 
					
					match rem1 with
						[] ->  AccRes (acc', em)
						| x1 :: xs1 -> p_star acc' rem1 p 
 *)


let parse_str str = 
	match str with
	[] -> ParseRes (Empty, [])
	| x :: xs -> ParseRes (Str x, xs) 


let parse_num num = 
	match num with
	[] -> ParseRes (Empty, [])
	| x :: xs -> ParseRes (Num x, xs) 



let lookahead_colon ls = 
	match ls with 
	[] -> false
	| x :: xs -> if (x = 58) then true else false

(*a simple version of key, a key is only a single char*)

(*Rid (out) = Rid(str) *)
let parse_key str = 
	if (str >= 97 && str <= 122) then 
		str 
	else
		raise ParseKeyEx

(*Rpmem(outpair) C Rmem (input)
/\ Robpair (outpair) C Robs(inp) *)
let rec parse_kvpair input = 
	match input with 
	[] -> ParseRes(Empty, [])
	| x :: xs ->  
			let colon = lookahead_colon xs in 
			if (colon = true) then 
				let key = parse_key x in 
				let value_rem = parse_value xs in 
				let value = projl value_rem in 
				let rem = projr value_rem in 
				let json = Obj (Pair (key, value)) in
				let res = ParseRes (json, rem) in 
				res
			else 
				raise (JSONTestEx "kv pair")


(* Rmemjson (out) C Rmem(inp) *)
and parse_value inp = 
	match inp with 
		[] -> raise (JSONTestEx "empty value")
		| x :: xs -> 
			(*if x is "{" then the value is Obj*)
			if (x = 123) then 
				let obj_res = parse_json inp in 
				let obj_value = projl obj_res in 
				let rem = projr obj_res in 
				ParseRes (obj_value , rem)
			else 
				if (x >= 97 && x <= 122) then 
					ParseRes (Str x, xs)
				else 
					ParseRes (Num x, xs)






(* 
parse_json : int list ->  json 
 constraints :
 	Rmemjson (out) C Rmem(ls) /\

	*)
and parse_json ls = 
 	match ls with 
	[] -> ParseRes (Empty, []) 
	| x :: xs ->
		(*either parse a string, or parse a number *) 
		if (x = 123) (*case {*) then 
		(* 	let  kvpairlist = p_star ([Empty]) xs parse_kvpair in 
		 *)	
		 	let  kvpair = parse_kvpair xs in 
		 	let rem = projr kvpair in 
		 	(match rem with (*test for terminal brace*)
		 		[] -> raise (JSONTestEx "empty remiander")
		 		| y :: ys -> 
		 			if (y = 125) then 
		 				kvpair
		 			else 

		 				raise (JSONTestEx ("no termianl }"^(string_of_int y))) )
		 			 		
		else
			if (x >= 97 && x <= 122) then (*tes for characters*)
				let strjson = parse_str ls in 
				strjson 
			else 
				if (x >= 48 && x <= 57) then  (*test for integers*)
					let numjson = parse_num ls in 
					numjson	  
				else 	
					raise (JSONTestEx "no json case") 	



(* 
let () = 
	(* {a : 0}*)
	let input = [123; 97; 58 ;49;125] in 
	
	let parse_res = parse_json input in 
	()
 *)