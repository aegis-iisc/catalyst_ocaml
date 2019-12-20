exception TestExp 
   
let raise ex = []



let rec loop l = 
	match l with 
		[] -> let temp = [] in 
				temp
		| x1 :: xs1 -> 
					 xs1

(*let rec loop n l = 
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
 *)					 	

(* 
let () = 
      
  		let src = [3;51;1;2] in 
        let parsedList  = loop 4 src  in 
        let () = List.iter (fun i -> Printf.printf "%s" (string_of_int i)) parsedList in 
        () 
 *)