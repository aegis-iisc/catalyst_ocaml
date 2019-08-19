let rec fold_right f l accum = 
        match l with 
        [] -> accum
      | x::xs -> let temp1 = f (x) (accum)  in 
                let temp = x :: accum in (*to aid the OCaml compiler infer the type of accum as a list*)
                let temp2 = fold_right (f) (xs) (temp1)  in 
                temp2
