

let rec concat l1 l2 = 
match l1 with
    [] -> let resem = l2 in resem
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                temp2



let hof ls = 
	match ls with 
	[] -> let em = [] in 
		let res0 = em in 
		res0 
	| x :: xs ->let em = [] in  
				let res = concat xs em  in 
				res 
 
let higher_order fho lst =
	let em = [] in 
	let lst' = concat lst em in  
	let reso = fho lst' in 
	let reso' = concat reso em in 
	reso 


let test lin = 
	let restest = higher_order hof lin in 
	restest