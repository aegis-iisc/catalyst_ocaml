exception JSONTestEx
exception ParseKeyEx

type json_value = Num of int 
				| Str of int 
				| Obj of json

and kv_pair = Pair of int * json_value 
		
and json = E 
			| L of kv_pair
			| LCons of kv_pair * json 


(*the return type for a parser (ParsedValue, Remaining List)*)
type json_rem  = JSONRes of (json) * (int list)
type jsonvalue_rem  = JSONValRes of (json_value) * (int list)
type extrachars_rem  = ExtraRes of (int) * (int list)


let projl lp = 
	match lp with 
		| ParseRes (l, r) -> l
	
let projr lp = 
	match  lp with
	| ParseRes (l, r) -> r




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


let parse_openbrace x = 
	if (x = 123) then 
		true 
	else 
		false 

let lookahead_colon ls = 
	match ls with 
	[] -> false
	| x :: xs -> if (x = 58) then true else false

(*a simple version of key, a key is only a single char*)
let parse_key str = 
	if (str >= 97 && str <= 122) then 
		str 
	else
		raise ParseKeyEx


let rec parse_kvpair input = 
	match input with 
	[] -> ParseRes(E, [])
	| x :: xs ->  
			let colon = lookahead_colon xs in 
			if (colon = true) then 
				let key = parse_key x in 
				let value_rem = parse_value xs in 
				let value = projl value_rem in 
				let rem = projr value_rem in 
				let json = L (Pair (key, value)) in
				let res = ParseRes (json, rem) in 
				res 


and parse_value inp = 
	match inp with 
		[] -> raise JSONTestEx
		| x :: xs -> 
			(*if x is "{" then the value is Obj*)
			if (x = 123) then 
				let extra_res = parse_openbrace x in 
				let obj_res = parse_json xs in 
				let obj_value = projl obj_res in 
				let rem = projr obj_res in 
				JSONValRes (Obj obj_value , rem)
			else 
				if (x >= 97 && x <= 122) then 
					JSONValRes (Str x, xs)
				else 
					JSONValRes (Num x, xs)






(* 
parse_json : int list -> json
 constraints : must start with a { 
	must end with a }
	*)
and parse_json ls = 
	[] -> E 
	| x :: xs ->
		(*either parse a string, or parse a number *) 
		if (x = 123) (*case {*) then 
			let openbrace = parse_openbrace x in   
			let  kvpairlist = pstar (E) parse_kvpair (openbrace) in 

		else 
			raise JSONTestEx 	