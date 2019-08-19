let rec concat l1 l2 = 
  match l1 with 
        [] -> l2 
  | x :: xs -> let temp = concat xs l2 in 
                x :: temp  

let rec rev l3 =
  match l3 with 
    [] ->  []
  | x::xs -> let temp2 = rev xs in
                let t1 = [] in         
                let temp3 = x::t1 in 
        
              let res= concat temp2 temp3 in 
                res

