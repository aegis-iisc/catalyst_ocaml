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
    [] -> let res0 = l2 in res0
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                temp2





(*trims the white spaces using many1 ws/tab*)

let rec parse_spaces ls =
	match ls with 
	[] -> let em = [] in 
		em
	| x :: xs -> 
		let thrty_two = 32 in 
		let eq_x_32 = eq x thrty_two in 
		if(eq_x_32) then 
			let trimmed = parse_spaces xs in 
			trimmed
			(* let one = 1 in
 			let em = [] in 
 			let one_l = one :: em  in 
			let rest = concat one_l trimmed in 
			rest *)  
		else
			let resf = ls in resf

(*the ws handled version*)
