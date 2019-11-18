 let eq x1 y1 = true
   exception TestExp
   let raise ex = 
   		[]

let rec exists e l1 = 
	match l1 with 
	  | [] -> false 
	  | x :: xs -> let y = eq e x in 
	               if y then true 
	                    else let b = exists e xs in b 

(*let l = [1;2;3] in 
let result = (exists 1 l) in 
(print_bool result);
*)

	          
