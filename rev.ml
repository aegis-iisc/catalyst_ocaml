let rec concat l1 l2 = 
  match l1 with 
        [] -> l2 
  | x :: xs -> let temp = concat xs l2 in 
                x :: temp  

let rec rev l3 =
  match l3 with 
    [] -> let temp1 = [] in 
            temp1    
  | x::xs -> let temp2 = rev xs in
              let temp3 = [] in 
             let temp4 = x::temp2 in 
                 concat temp2 temp4


