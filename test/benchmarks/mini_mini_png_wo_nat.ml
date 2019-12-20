exception TestExp 
(*dummy functions from Pervasive*)   
let raise ex = [] 
let eq x1 y1 = true
let minus_one n1 = n1

(*Internal function to iterate over a list
 @param n : size of the following input list
 @param l : input list 
 @return : the same list as the input
 @raises : TestExp if n and l's size mismatch
 @property : n -> {l1} -> {v1 | Rlen(v1) = (n) /\ Rlen(l1) = (n) }
*)
let rec loop n l = 
	match l with 
		[] -> 
			let z = 0 in 	
			let eq_n_z = eq n z in 
			if eq_n_z then  
				[]  
				
			else 
				raise TestExp
		| x :: xs ->
			let n_min_1 = minus_one n in 
			let res = loop n_min_1 xs in
			let out = x :: res in 
			out 			
(* a simple parsing function
	@param inp : input list 
	@return : a parsed list, same as input 
	@property : inp ->  {v | Rhd(inp) = {()} \/ Rhdn (inp) = Rlentail (v)}
*)	
let parse_png inp  = 
	match inp with 
		[] -> let temp = [] in 
				temp
		| x1 :: xs1 -> 
					 let z1 = 0 in 
					 let eq_x1_z1 = eq x1 z1 in 	
				     if (eq_x1_z1)
 					 then 
 					 	raise TestExp
 					 else
 					 	let parsed = loop x1 xs1 in 
					 	parsed
					 	(* let res = x1 :: parsed in 
					 	res *)
(*test*) 
(*
let () = 
      let src = [3;51;1;2] in 
      let parsedList  = parse_png src  in 
      let () = List.iter (fun i -> Printf.printf "%s" (string_of_int i)) parsedList in 
        ()  

 *)
