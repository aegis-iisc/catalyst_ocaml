(*A CSV parser *)

type inputchars = I of int| S of string | C of char

type json = | Num of int 
			| Str of string 
			| Obj of obj
			| Empty 
type state = (string list) * int 

type obj = Triple of list state (*list has unique names, and int is greater than 100*)

type output = state * json * (inputchars list) 

(*parse_json : input : inputchars -> json *)
let parse_json input nameset agesum = 
		match input with 
		[] -> raise Error
		| x :: xs -> 
			match x with 
			I _  -> (*Num case*)
				parse_number input

			| S _ -> (*String case*)
				parse_string input 
			| C _ -> 			
				let obj = parse_obj input nameset agesum in 
				Triple obj

(*takes an input and a nameset, and returns a name such that name is unique wrt the nameset, and also needs to prove that global pointer is updated correctly , further the remaining is a substring of the input and name is a correct parse for the name grammar*)
let name_value input nameset = 
	match input with 
		[] -> raise Error 
		| x :: xs -> 
			let (name, r1 ) = parse_char input in 
			if (present name nameset)
				then let new_name = String.concat name (string_of global_counter) in 
				let new_nameset = new_name :: nameset in  
				((new_name, new_nameset), r1)
			else 
				let new_nameset = name :: nameset in 	
				((name, new_nameset),  r1)


let age_value input agesum = 
	match input with 
		[] -> raise Error 
		| x :: xs -> 
			let (age, r1 ) = parse_int input in 
			let new_agesum = agesum + age in 	
				((age, new_agesum), r1)



let colon input nameset agesum = 
	match input with 
	[] -> raise Error
	| x :: xs -> 
		match x with 
			| C ':' -> 


(*parse_obj : input -> nameset-> agesum -> obj | such that , list is having unique names, age is the sum of ages of age field, remaining is sublist of input and rem-input is correct representation for the grammar for obj*)
let rec parse_obj input nameset agesum = 
	match input with 
		[] -> Triple (nameset, age_sum) 
		| x :: xs -> 
			let (v1, r1) = left_bracket input in 
			let (v2, r2) = open_brace r1 in 
			let (_, r3)	 = name_key r2 in 
			let (_, r4)	 = colon r3 in 
			let ((name_value, nameset'), r5) = name_value r4 nameset in 
			let  let (_, r6)	 = age_key r5 in 
			let (_, r7)	 = colon r6 in 
			let ((age_value, age_sum'), r8) = age_value r7 agesum in 
			let (_, r9) = open_brace r8 in 
			parse_obj r9 nameset' agesum


				
let () = 
	let nameset0 = [] in 
	let age_sum0 = 0 in 		
	let inp = [C '[' ; S "{" ; S "name"  ]	