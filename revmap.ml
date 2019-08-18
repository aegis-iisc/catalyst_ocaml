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

let rec map f l = 
        match l with 
        [] -> []
      | x::xs -> let tm1 = f x in 
                let tm2 = map f xs in 
                let res =  tm1 :: tm2 in 
                res

let revmap f l = 
      let mapped = map f l in 
      let res = rev mapped in 
      res