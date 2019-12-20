exception TestExp of string 
exception AssertionFailed
type bt = 
	| Leaf 
	| Node of int * bt * bt

type input_chars = 
		| Char of string 
		| Int of int 


type output = Pair of bt * input_chars list
 

let check b = 
		if b then () 
			else raise AssertionFailed 

let projl lp = 
	match lp with 
		| Pair (l, r) -> l
	
let projr lp = 
	match  lp with
	| Pair(l, r) -> r


let rec string_of_inp ls = 
    match ls with 
      []-> "[]"
      | x :: xs -> 
      		match x with 
      			| Char s ->  
		      	"[ "^(s)^" : "^(string_of_inp xs)^" ]" 
		      	| Int i -> 	
		      	"[ "^(string_of_int i)^" : "^(string_of_inp xs)^" ]" 

let rec string_of_bt bt = 
		match bt with 
			| Leaf -> " <leaf> "
			| Node (i, l, r)   -> " ( Node ( "^(string_of_int i)^" * "^(string_of_bt l)^" * "^(string_of_bt r)^" ) )"

let rec string_of_output p = 
	match p with 
		Pair (l1, l2) -> "{ parsed "^(string_of_bt l1)^", \n remaining "^(string_of_inp l2)^" }" 




let consume_element l  = 
	let () = Printf.printf "%s" " \n parse_empty " in
	
	match l with 
		| [] ->  []
		| x :: xs -> xs 


let parse_leaf_value l =
	let () = Printf.printf "%s" " \n parse_leaf_value " in
	match l with 
		[] -> raise (TestExp "Empty Leaf")
		| x :: xs -> 
				match x with 
				 Char s -> raise (TestExp "Leaf value cannot be a string")
				 | Int x -> 
						if x = 0 then 	
						let res = Pair (Leaf, xs) in 
						res
					else 
						raise (TestExp "Invalid Leaf") 


let parse_leaf ls = 
	let () = Printf.printf "%s" " \n parse_leaf " in
	match ls with 
		[] ->  raise (TestExp "Empty Leaf")
		| x :: xs -> 
			match x with 
			Char s ->
				 if (s = "<") then 
					let leaf_pair  = parse_leaf_value xs in 
					let leaf_el = projl leaf_pair in 
					let leaf_rem = projr leaf_pair in 
					let rem = consume_element leaf_rem in 
					Pair (Leaf, rem)
				else 
						raise (TestExp "Invalid character symbol")
			| _ -> raise (TestExp "Not a leaf")	 
let rec parse_bt  ls =
	let () = Printf.printf "%s" " \n parse_bt " in 

	match ls with 
		[] -> raise (TestExp "A BT must not be empty")
		| x :: xs -> 
			match x with 
				Char s -> (*parse leaf*)
					parse_leaf ls 
				| Int i -> (*parse internal node*) 
					parse_node ls



and parse_node ls = 
 		let () = Printf.printf "%s" "\n parse_node " in 
				
		match ls with
			[] -> raise (TestExp "Node must be non-empty") 
			| x :: xs ->
				match x with 		 
				 | Int i -> 
					 let node = i in 
					 let btl_pair = parse_bt xs in 
					 let rem = projr btl_pair in 
					 let btr_pair = parse_bt rem in
					 let btl = projl btl_pair in 
					 let btr = projl btr_pair in
					 let rem_f = projr btr_pair in 
					 let () = Printf.printf "%s" "\n remaining node \n" in 
					 let () = Printf.printf "%s" (string_of_inp rem_f) in 
					 match rem_f with 
						[] -> Pair ( Node (node, btl, btr), rem_f) 
						| x1 :: xs1 -> parse_bt rem_f   
										
								  
										
				 | _ -> raise (TestExp "node value must be an int")	




let () = 
      (* let src =  [ Int 2 ; Char "<" ; Int 0 ; Char ">" ; Int 4 ; Char "<" ; Int 0 ; Char ">" ; Char "<" ; Int 0 ; Char ">" ] in 
       *)(* let src =  [ Int 2 ; Char "<" ; Int 0 ; Char ">" ; Int 4 ; Char "<" ; Int 0 ; Char ">" ; Char "<" ; Int 0 ; Char ">" ] in  *)
      let src =  [ Int 10 ;
      					Int 6 ;
      						Int 4 ; Char "<" ; Int 0 ; Char ">" ; Char "<" ; Int 0 ; Char ">" ;
      						Int 8 ; Char "<" ; Int 0 ; Char ">" ; Char "<" ; Int 0 ; Char ">" ;

      					Int 9; Char "<" ; Int 0 ; Char ">" ; Char "<" ; Int 0 ; Char ">" ] in 
      
      let parsedList  = parse_bt src  in 
      let () = Printf.printf "%s" ("\n test1 ") in 
      let () = Printf.printf "%s" ("\ninput list "^(string_of_inp src)) in 
      let () = Printf.printf "%s" ("\nparsed list "^(string_of_output parsedList)) in 
     
        ()  