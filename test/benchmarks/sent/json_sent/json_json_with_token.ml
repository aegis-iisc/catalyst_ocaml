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

let eq x1 y1 = if (x1 = y1) then true else false
 *)

let eq x1 y1 = true 

let raise ex = 
		let em = [] in 
		let z = 0 in 
		let defjson = Num z in 
		let defrem = em in  
		let resex = ParseRes (defjson,defrem) in 
		resex


let rec concat l1 l2 = 
match l1 with
    [] -> let resem = l2 in resem
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                temp2



(* 
let parse_str str = 
	match str with
	[] -> raise (JSONEx "Invalid string")
	| x :: xs -> 
			let jsres = Str x in 
			ParseRes (jsres, xs) 
 *)



(*trims the white spaces using many1 ws/tab*)

let rec parse_spaces ls =
	match ls with 
	[] -> let em = [] in em
	| x :: xs -> 
		let thrty_two = 32 in 
		let eq_x_32 = eq x thrty_two in 
		if(eq_x_32) then 
			let trimmed = parse_spaces xs in 
			trimmed  
		else
			let rese = ls in 
			rese	




let parse_token (fwsp) (lst) (inparser) = 
	let em = [] in 
	let lst = concat lst em in 
	let lst' = fwsp lst in
	let lst'' = concat lst' em in 
	let res = inparser lst'' in 
	let l1 = projl res in 
	let l2 = projr res in 
	res  





let parse_num inp = 
	match inp with
	[] -> raise (JSONEx "Invalid number")
	| x :: xs ->
				(* let two = 2 in 
				let jsres = Num two in 	 
				 *)let jsres = Num x in 
	 			 let restoret = ParseRes (jsres, xs) in 
				restoret
 		


let parse_num_ref lnum = 
	let js = parse_token (parse_spaces) (lnum) (parse_num) in 
(* 	let rem = projr js in 
	let two = 2 in 
	let jsdummy = Num two in 
	let resjs = ParseRes (jsdummy, rem) in 
	resjs
 *)
 	js
let parse_colon ls = 
	match ls with 
	[] -> raise (JSONEx "Error parsing colon no input ")
	| x :: xs -> 
			let fifty_eight = 58 in 
			let eq_x_58 = eq x fifty_eight in 
			if (eq_x_58) 
			then 
				let colon_parse = Str x in
				(* let two = 2 in 
				let colon_parse = Str two in   
				 *)let resc = ParseRes (colon_parse, xs) in 
				resc
			else 
				raise (JSONEx ("Error parsing colon found"))

let parse_colon_ref lls = 
	let colonres = parse_token (parse_spaces) (lls) (parse_colon) in 
	colonres 


let rec parse_field fls = 
	match fls with 
	[] -> raise (JSONEx "parsing empty field")
	| x :: xs -> 
		let key_n_r1 = parse_json_ref fls in 
		let key = projl key_n_r1 in 
		let rem1 = projr key_n_r1 in 
		let colon_n_r2 = parse_colon_ref rem1 in 
		let rem2 = projr colon_n_r2 in 
		let value_n_r3 = parse_json_ref rem2 in 
		let value = projl value_n_r3 in 
		let rem4 = projr value_n_r3 in 
		
		let f = Pair (key, value) in 
		let json4f = Obj f in 
		(* let em = [] in 
		let z = 0 in 
		let defjson = Num z in 
		 *)
		let resout = ParseRes (json4f, rem4)   in 
		resout


and  parse_json inlist = 
	match inlist with 
	[] -> 	raise (JSONEx "parsing empty")
	| x :: xs ->
		(*either parse a string, or parse a number *) 
		let one_twnty_three = 123 in 
		let eq_x_123 = eq x one_twnty_three in 
		if (eq_x_123) (*case {*) then 
			let  f = parse_field_ref xs in 
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
		   let numjson = parse_num_ref inlist in 
			numjson	  
 
 and parse_json_ref rinlist = 
 	let jsres = parse_token (parse_spaces) (rinlist) (parse_json) in 
 	jsres			
and parse_field_ref rfls = 
	let jsfield = parse_token (parse_spaces) (rfls) (parse_field) in 
	jsfield

(*the ws handled version*)

(* 

let () = 
	(* {a : 0}*)
	let test0 = [123;97;58;49;125] in 
	let res0 = parse_json_ref test0 in

	let test1 = [32;32;123;97;58;49;125] in 
	let res1 = parse_json_ref test1 in
 	
	let test2 = [32;32;123;32;32;97;58;49;125] in 
	let res2 = parse_json_ref test2 in
 	
 	let test3 = [32;32;123;32;32;97;32;32;58 ;49;125] in 
	let res3 = parse_json_ref test3 in
 	

 	let test4 = [32;32;123;32;32;97;32;32;58 ;32;32;32;49;125] in 
	let res4 = parse_json_ref test4 in


 	let test5 = [32;32;123;32;32;97;32;32;58;49;32;32;32;125] in 
	let res5 = parse_json_ref test5 in
 	let () = Printf.printf "%s" ("\n test0 ") in 
    let () = Printf.printf "%s" ("\ninput list "^(intlist_to_string test0)) in 
    let () = Printf.printf "%s" ("\n output "^(output_to_string res0)) in  
    

    let () = Printf.printf "%s" ("\n test1 ") in 
    let () = Printf.printf "%s" ("\ninput list "^(intlist_to_string test1)) in 
    let () = Printf.printf "%s" ("\n output "^(output_to_string res1)) in  
    
    let () = Printf.printf "%s" ("\n test2 ") in 
    let () = Printf.printf "%s" ("\ninput list "^(intlist_to_string test2)) in 
    let () = Printf.printf "%s" ("\n output "^(output_to_string res2)) in  
    
    let () = Printf.printf "%s" ("\n test3 ") in 
    let () = Printf.printf "%s" ("\ninput list "^(intlist_to_string test3)) in 
    let () = Printf.printf "%s" ("\n output "^(output_to_string res3)) in  
    
    let () = Printf.printf "%s" ("\n test4 ") in 
    let () = Printf.printf "%s" ("\ninput list "^(intlist_to_string test4)) in 
    let () = Printf.printf "%s" ("\n output "^(output_to_string res4)) in  
    
    let () = Printf.printf "%s" ("\n test5 ") in 
    let () = Printf.printf "%s" ("\ninput list "^(intlist_to_string test5)) in 
    let () = Printf.printf "%s" ("\n output "^(output_to_string res5)) in  
     
	()
 *)