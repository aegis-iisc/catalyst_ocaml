type bit = Zero | One 
type pair = Pair of bit * bit 
type list_pair =
			E 
			| L of pair 
			| LCons of pair * list_pair



let xor_b p = 
	match p with 
	Pair (b1, b2) ->
		match b1 with 
			| Zero -> match b2 with 
					Zero -> 0  
					| One -> 1
			| One -> match b2 with 
					Zero -> 1 
					| One -> 0		


 
let rec map f ls = 
  match ls with 
    | E -> []
    | L p -> let f_p = f p in 
    	  let empty = [] in 
          let res = f_p :: empty in 
          res  
    | LCons (p, pl) ->
           let f_p = f p in
           let  f_pl = map f pl in    
           let res = f_p :: f_pl in 
                res

let xor pl = 
	let res = map xor_b pl in 
	res

