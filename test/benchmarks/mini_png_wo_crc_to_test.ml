exception TestExp of string 



(*the return type for a parser (ParsedValue, Remaining List)*)
type listPair  = Pair of (int list) * (int list)


let projl lp = 
	match lp with 
		| Pair (l, r) -> l
	
let projr lp = 
	match  lp with
	| Pair(l, r) -> r

		
(* uncomment to typecheck*) 
(* let eq x1 y1 = true
let raise ex = [] *)

(*printing utilities, comment to typecheck*)
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



(*Internal function to iterate parse a single chunk
  @param ac: accumulator list 
  @param n : size of the following input list
  @param l : input list 
  @return : 
  @raises : TestExp if n and l's size mismatch
  @property : {ac: list} -> n -> {l1} -> {v1 | Rlen(v1) = (n) /\ Rlen(l1) = (n)  /\ Rmem Rfst(v) C ( Rmem(ac) U Rmem (l1)) }
                          *)
let rec parse_chunk ac n l = 
  match l with 
    [] -> 
        if (n=0) then 
        	Pair (ac,[])  
      	else 
      		let () = Printf.printf "%s" (string_of_int n) in 
        	raise ( TestExp "Returning empty for n != 0")
  | x :: xs ->
      let n_min_1 = n-1 in 
      if (n= 0) then 
      		Pair (ac,l)
      else 
      let ac' = concat ac [x] in 
      let res = parse_chunk ac' n_min_1 xs in
      res

 

(* A parser for a sequence of chunks *)
(* parse_chunk_star : { acc : int list } -> {inpc : int list} -> {v | Rmem Rfst(v) C ( Rmem(acc) U Rmem (inpc))} *)
(*A limitation of the current system-  
	we cannot say a property like, sum of the length fields of all the chunks = length of the output for chunk_star*)
let rec parse_chunk_star acc inpc = 
	match inpc with 
		[] -> Pair (acc, [])
		| x :: xs ->  
					let chunk_i = parse_chunk [] x xs in 
					let p1 = projl (chunk_i) in
					let rem1 = projr (chunk_i) in 
					let acc' = concat acc p1 in 
					
					match rem1 with
						[] ->  Pair (acc', [])
						| x1 :: xs1 -> parse_chunk_star acc' rem1 


(* A parser for a given header and an input list 
parse_header : hd -> inp ->  {v | [v=true] <=> Rhd(inp) = {(hd)}*)
 let parse_header hd inp = 
	match inp with
	[] -> raise (TestExp "Missing Header")
	| x :: xs -> 
		 if (x = hd) then 
		 	true 
		 else
		 	false


(*	@retrun : Pair (parsedlist, remaining)
	parse_png : l1 -> (acc : list -> 
						in : list -> 
							{z | Rmem Rfst(z) C ( Rmem(acc) U Rmem (in))}
					) -> 
					{v | Rlen Rfst (v) C Rlen (l1) /\
							Rmem Rfst (v) C Rmem (l1) /\
							Robs Rfst (v) C Robs (l1) 
						 	Rmem Rsnd (v) = {()} 
					}*)
let parse_png hd inp f = 
	match inp with 
		[] -> raise ( TestExp "Missing Header")
		| x :: xs -> 
					let h = parse_header hd inp in
					if(h=true) then  
						let chunks = f [] xs in 
						let () = Printf.printf "%s" (" chunks "^(string_of_pair chunks)) in 
						let res = projl chunks in 
						let rem = projr chunks in 
						Pair (res, rem)
					 else 
					 	raise ( TestExp "Incorrect Header") 	


 
(*@param src : list header, followed by a data, such that header gives the length of the remaining data*)
      
let ()  = 
	  let src = [88;2;1;2;1;4] in 
      
      let parsedList  = parse_png 88 src parse_chunk_star  in 
      let () = Printf.printf "%s" ("\n test1 ") in 
      let () = Printf.printf "%s" ("\ninput list "^(string_of_intlist src)) in 
      let () = Printf.printf "%s" ("\nparsed list "^(string_of_pair parsedList)) in 
     
        ()  

