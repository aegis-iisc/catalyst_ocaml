let rec fold_left f accum l = 
        match l with 
        [] -> accum
      | x::xs -> let temp1 = f (accum) (x) in 
                let temp = x :: accum in (*to aid the OCaml compiler infer the type of accum*)
                let temp2 = fold_left (f) (temp1) (xs)  in 
                temp2
                
