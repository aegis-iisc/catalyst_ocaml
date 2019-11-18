let eq x1 y1 = true
   
exception TestExp
   let raise ex = 
   		[]

let rec remove_element x l = match l with 
  | [] -> []
  | y :: ys -> let z = eq x y in
               let result = remove_element x ys in 
               if z then result 
                    else y :: result