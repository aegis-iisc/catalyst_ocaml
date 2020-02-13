let rec map f l = 
        match l with 
        [] -> let res0 = l in res0
      | x::xs -> let tm1 = f x in 
                let tm2 = map f xs in 
                let res =  tm1 :: tm2 in 
                res
