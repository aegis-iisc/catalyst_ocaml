let rec concat l1 l2 = 
match l1 with
    [] -> let res0 = l2 in 
    		res0
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                (* let tt = true in 
                if (tt) then 
                	let rest = x1 :: temp1 in 
                	rest
                else 
                 *)	let resf = x1 :: temp1 in 
                	resf 
(*Incorrect implementation*)
(* match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                        temp1 
 *)