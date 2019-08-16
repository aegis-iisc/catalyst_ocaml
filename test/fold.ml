let rec fold_left f accum l = 
        match l with 
        [] -> accum
      | x::xs -> let temp1 = f (accum) (x) in 
                 fold_left (f) (temp1) (xs)  
                
(*let fFun lacc y = 

        y :: []
let () =
  let lin = [1;2;3] in 
  let 
  *)
