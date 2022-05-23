(*A-normalform is required*)
let rec append l1 l2  = 
  match l1 with 
  [] -> let t = l2 in t 
  | x::xs -> let t1 = append xs l2 in 
             let t2 = x::t1 in t2 
