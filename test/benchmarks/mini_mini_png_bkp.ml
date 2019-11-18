exception TestExp 

 
let eq x1 y1 = (x1 = y1) 

let lt x1 y1 = (x1 < y1)

 (* 
let raise ex = [] 
 *)


(*The size of the input must be less than the parameter n*)
(*parse_chunk : {n : int | n >= 0} -> {l1 | Rlen (l1) = n} -> {v | Rlen (v) = n  

(*does not typechek as constraint for n>= 0 not enforced*)														}*)
let rec parse_chunk_cksum n inpc  =
	match inpc with 
		[] -> 
			let n_eq_0 = eq n 0 in 
			 if n_eq_0 then  
				[]
			else 
			  	raise TestExp	
		| x :: xs ->   let t1= parse_chunk_cksum (n-1) xs  in 
					  let res = x :: t1 in
					  	res
					 
(*How to encode the grammar of the png in the specification??*)
(*parse_png : l1 -> {v | 	 
							Rlen (v) = Rlen (l1) /\
							Rmem (v) = Rmem (l1) /\
							Robs (v) = Robs (l1) }*)
let parse_png inp  = 
	match inp with 
		[] -> []
		| x :: xs -> let len = x in
 					 if (len >= 0)
 					 then 
 					 	let chunk_n_cksum = parse_chunk_cksum (len+1) xs  in
					 	let res = x :: chunk_n_cksum in 
					 	res
					 else 
					   raise TestExp	

let () = 
      
  		let src = [3;51;1;2;3] in 
        let parsedList  = parse_png src  in 
        let () = List.iter (fun i -> Printf.printf "%s" (string_of_int i)) parsedList in 
        () 

 