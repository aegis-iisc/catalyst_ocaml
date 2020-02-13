exception JSONEx of string 
type json = 
			| Num of int 
			| Str of int 
			| Obj of field 
and field = Pair of json * json 

type output  = ParseRes of (json) * (int list)

(* 
let rec json_to_string j = 
	match j with 
	| Num i -> ("Num "^(string_of_int i)) 
	| Str s -> ("Str "^(string_of_int s)) 
	| Obj f -> ("Obj "^(field_to_string f)) 

and field_to_string f = 
	match f with 
	| Pair (j1, j2) -> ("{ "^(json_to_string j1)^" : "^(json_to_string j2)^" }")


let rec intlist_to_string ls = 
    match ls with 
      []-> "[]"
      | x :: xs -> "[ "^(string_of_int x)^" : "^(intlist_to_string xs)^" ]" 	 	
 *)
(*proj for Pair (json, int list)*)
let projl lp = 
	match lp with 
		| ParseRes (l, r) -> l
	
let projr lp = 
	match  lp with
	| ParseRes (l, r) -> r

(* 

let output_to_string o = 
	let j = projl o in 
	let r = projr o in 
	let out_str = ("ParseRes { "^(json_to_string j)^" , \t "^(intlist_to_string r)^" }") in 
	out_str
 *)
(* let eq x1 y1 = if (x1 = y1) then true else false
 *)

let eq x1 y1 = true 

let raise ex = 
		let em = [] in 
		let z = 0 in 
		let defjson = Num z in 
		let defrem = em in  
		let resex = ParseRes (defjson,defrem) in 
		resex

(* 
let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                temp2
 *)


(* 
let parse_str str = 
	match str with
	[] -> raise (JSONEx "Invalid string")
	| x :: xs -> 
			let jsres = Str x in 
			ParseRes (jsres, xs) 
 *)

let parse_num num = 
	match num with
	[] -> raise (JSONEx "Invalid number")
	| x :: xs ->
				let jsres = Num x in 	 
				ParseRes (jsres, xs) 



let parse_colon ls = 
	match ls with 
	[] -> raise (JSONEx "Error parsing colon no input ")
	| x :: xs -> 
			let fifty_eight = 58 in 
			let eq_x_58 = eq x fifty_eight in 
			if (eq_x_58) 
			then 
				let colon_parse = Str x in 
				let res = ParseRes (colon_parse, xs) in 
				res
			else 
				raise (JSONEx ("Error parsing colon found"))

 

let rec parse_field fls = 
	match fls with 
	[] -> raise (JSONEx "parsing empty field")
	| x :: xs -> 
		let key_n_r1 = parse_json fls in 
		let key = projl key_n_r1 in 
		let rem1 = projr key_n_r1 in 
		let colon_n_r2 = parse_colon rem1 in 
		let rem2 = projr colon_n_r2 in 
		let value_n_r3 = parse_json rem2 in 
		let value = projl value_n_r3 in 
		let rem4 = projr value_n_r3 in 
		
		let f = Pair (key, value) in 
		let json4f = Obj f in 
		(* let em = [] in 
		let z = 0 in 
		let defjson = Num z in 
		 *)
		ParseRes (json4f, rem4)   


and  parse_json inlist = 
	match inlist with 
	[] -> 	raise (JSONEx "parsing empty")
	| x :: xs ->
		(*either parse a string, or parse a number *) 
		let one_twnty_three = 123 in 
		let eq_x_123 = eq x one_twnty_three in 
		if (eq_x_123) (*case {*) then 
			let  f = parse_field xs in 
		 	let rem = projr f in 
		 	(match rem with (*test for terminal brace*)
		 		[] -> raise (JSONEx "End reached without terminal }")
		 		| y :: ys -> 
					let one_twnty_five = 125 in 
					let eq_x_125 = eq y one_twnty_five in 
		 			if (eq_x_125) then 
		 				f
		 			else 
  	 				  raise (JSONEx ("no termianl }")) )
		 			 		
		else
			(* if (x >= 97 && x <= 122) then (*tes for characters*)
				let strjson = parse_str inlist in 
				strjson 
			else 
				if (x >= 48 && x <= 57) then  (*test for integers*)
	 *)		let numjson = parse_num inlist in 
			numjson	  
			



(* 
let () = 
	(* {a : 0}*)
	let input = [123;97 ; 58 ;49;125] in 
	let res = parse_json input in
 	let () = Printf.printf "%s" ("\n test1 ") in 
    let () = Printf.printf "%s" ("\ninput list "^(intlist_to_string input)) in 
    let () = Printf.printf "%s" ("\n output "^(output_to_string res)) in 
     
	()
 *)