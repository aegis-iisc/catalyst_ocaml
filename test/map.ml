let rec map f l = 
        match l with 
        [] -> l
      | x::xs -> let tm1 = f x in 
                let tm2 = map f xs in 
                let res =  tm1 :: tm2 in 
                res
