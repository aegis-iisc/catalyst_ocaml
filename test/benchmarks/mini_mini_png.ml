exception TestExp 
   
let raise ex = [] 

type nat = 
	| Z 
	| S of nat

let eq x1 y1 = true
let minus_one n1 = n1


let rec loop n l = 
	match l with 
		[] -> 
			let z = 0 in 	
			let eq_n_z = eq n z in 
			if eq_n_z then  
				let res1 = [] in 
				res1
			else 
				raise TestExp
		| x :: xs ->
			let n_min_1 = minus_one n in 
			let res = loop n_min_1 xs in
			let out = x :: res in 
			out 			
					 	
let parse_png inp  = 
	match inp with 
		[] -> let temp = [] in 
				temp
		| x1 :: xs1 -> let len = x1 in
					 let z1 = 0 in 
					 let eq_len_z1 = eq len z1 in 	
				     if (eq_len_z1)
 					 then 
 					 	raise TestExp
 					 else
 					 	let parsed = loop len xs1 in 
					 	parsed

(* 
let () = 
      
  		let src = [3;51;1;2] in 
        let parsedList  = parse_png src  in 
        let () = List.iter (fun i -> Printf.printf "%s" (string_of_int i)) parsedList in 
        ()  *)