exception TestExp 



(*the return type for a parser (ParsedValue, Remaining List)*)
type listPair  = Pair of (int list) * (int list)


let projl lp = 
	match lp with 
		| Pair (l, r) -> l
	
let projr lp = 
	match  lp with
	| Pair(l, r) -> r

(*Pervasive library functions, comment to typecheck, uncomment to excute the tests*)
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

let rec string_of_pair p = 
	match p with 
		Pair (l1, l2) -> "{ parsed "^(string_of_intlist l1)^", \n remaining "^(string_of_intlist l2)^" }" 


let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                        temp2 



(*Internal function to parse a single chunk
  @param ac: accumulator list 
  @param n : size of the following input list
  @param l : input list 
  @raises : TestExp if n and l's size mismatch
  example behavior : 
  (using alphabets for ease of explaining, we do not have support for chars so both the length and data are int) 
  	ac = [z] ->  n = 2 -> l = [a;b;c;d] = Pair ([z;a;b], [c;d])
    ac = [z] ->  n = 2 -> l = [a] = Exception 
    ac = [] ->  n = 2 -> l = [a;b;c;d] -> Pair ([a;b], [c,d] *)
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

 
 
(* A parser for a sequence of chunks *)
(* parse_chunk_star : { acc : int list } -> {inpc : int list} -> (p: ?) -> Pair (parsed, remaining)} 
 example behavior :
 (using alphabets for ease of explaining, we do not have support for chars so both the length and data are int) 

   acc = [x;y] -> inpc = [3;a;b;c;2;d;e] -> p = (parse_chunk) => out = [x;y;a;b;c;d;e]
   acc = [x;y] -> inpc = [3;a;b;c;2;d;e;f] -> p = (parse_chunk) => out = Exception *)



(*A limitation of the current system-  
	we cannot say a property like, sum of the length fields of all the chunks = length of the output for chunk_star + the len of acc
	ex- 
	acc = [x;y] -> inpc = [3;a;b;c;2;d;e] -> p = (parse_chunk) => out = [x;y;a;b;c;d;e]
	len (out) = 3 + 2 + len (acc)
*)

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


(* A parser for a given header and an input list 
parse_header : hd -> inp ->  {v | [v=true] <=> Rhd(inp) = {(hd)}*)
 let parse_header hd inp = 
	match inp with
	[] -> false 
	| x :: xs ->
		 let x_eq_hd = eq x hd in  
		 if (x_eq_hd) then 
		 	true 
		 else
		 	false


(*	@retrun : Pair (parsedlist, remaining)*)
(*hd : int -> inp : int list -> (optional, chunk_star parser f : ?) -> (parsed : int list * remaining * int list) *)					
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


 
(*@param src : list header, followed by a data, such that header gives the length of the remaining data*)
      
let ()  = 
	  let src = [88;3;1;2;1;3;2;3;4;] in 
      
      let parsedList  = parse_png 88 src  in 
      let () = Printf.printf "%s" ("\n test1 ") in 
      let () = Printf.printf "%s" ("\ninput list "^(string_of_intlist src)) in 
      let () = Printf.printf "%s" ("\nparsed list "^(string_of_pair parsedList)) in 
     
        ()  

