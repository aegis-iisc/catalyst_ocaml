let rec append l1 l2  = 
  match l1 with 
  [] -> l2 
  | x::xs -> let t1 = append xs l2 in 
                
              let t2 = x::t1 in 
                t2 
