let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | 1::xs1 ->  let temp1 = concat xs1 l2 in
                let const1 = 1 in 
                let temp2 = const1::temp1 in 
                        temp2
