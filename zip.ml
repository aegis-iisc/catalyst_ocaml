type ('a, 'a) pair = Pair of 'a * 'a 

let rec zip l1 l2 = 
        match (l1, l2) with 
        ([], []) -> []
      | (x1::xs1, x2::xs2) -> (Pair (x1,x2)::zip (xs1, xs2))            
