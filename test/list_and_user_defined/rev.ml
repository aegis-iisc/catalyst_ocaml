let rec concat l1 l2 = 
  match l1 with 
        [] -> l2 
  | x :: xs -> let temp = concat xs l2 in 
        let  concatnated  = x :: temp  in 
        concatnated

let rec rev l3 =
  match l3 with 
    [] ->  []
  | x1::xs1 -> let temp2 = rev xs1 in
                let t1 = [] in         
                let temp3 = x1::t1 in 
        
              let res= concat temp2 temp3 in 
                res

