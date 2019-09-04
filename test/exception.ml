exception TestExp 


let gt x1 y1 = 
	true

let raise ex = 
	-1

let compare x y =  
	let compre = gt x y in 
	if (compre) then 
		x 
	else 
		let va = TestExp in 
		let res = raise va in 
		res
(* 	
let compare2 x y =  
	let compre = x > y in 
	if (compre) then 
		x 
	else 
		raise TestExp
 *)

(* 
let () = 
	let res = foo  3 2 in 
	let () = Printf.printf "%s" ("Res is "^(string_of_bool res)) in 
	()
 *)
