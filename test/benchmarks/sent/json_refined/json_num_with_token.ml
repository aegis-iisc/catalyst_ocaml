exception JSONEx of string 
type json = 
			| Num of int 
			| Str of int 
			| Obj of field 
and field = Pair of json * json 

type output  = ParseRes of (json) * (int list)

(*proj for Pair (json, int list)*)
let projl lp = 
	match lp with 
		| ParseRes (l, r) -> l
	
let projr lp = 
	match  lp with
	| ParseRes (l, r) -> r


let eq x1 y1 = true 

let raise ex = 
		let em = [] in 
		let z = 0 in 
		let defjson = Num z in 
		let defrem = em in  
		let resex = ParseRes (defjson,defrem) in 
		resex


let rec concat l1 l2 = 
match l1 with
    [] -> let resem = l2 in resem
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                temp2





(*trims the white spaces using many1 ws/tab*)

let rec parse_spaces ls =
	match ls with 
	[] -> let em = [] in em
	| x :: xs -> 
		let thrty_two = 32 in 
		let eq_x_32 = eq x thrty_two in 
		if(eq_x_32) then 
			let trimmed = parse_spaces xs in 
			trimmed  
		else
			let rese = ls in 
			rese	




let parse_token (fwsp) (lst) (inparser) = 
	let em = [] in 
	let lst = concat lst em in 
	let lst' = fwsp lst in
	let lst'' = concat lst' em in 
	let res = inparser lst'' in 
	let l1 = projl res in 
	let l2 = projr res in 
	res  





let parse_num inp = 
	match inp with
	[] -> raise (JSONEx "Invalid number")
	| x :: xs ->
				(* let two = 2 in 
				let jsres = Num two in 	 
				 *)let jsres = Num x in 
	 			 let restoret = ParseRes (jsres, xs) in 
				restoret
 		


let parse_num_ref lnum = 
	let js = parse_token (parse_spaces) (lnum) (parse_num) in 
(* 	let rem = projr js in 
	let two = 2 in 
	let jsdummy = Num two in 
	let resjs = ParseRes (jsdummy, rem) in 
	resjs
 *)
 	js


(*the ws handled version*)

