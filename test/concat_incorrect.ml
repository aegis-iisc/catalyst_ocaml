let rec concat l1 l2 = 
 match l1 with
    [] -> let t = l2 in t
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in []
 *