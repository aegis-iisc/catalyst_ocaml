 let eq x1 y1 = true
   exception TestExp
   let raise ex = 
   		[]

 let rec filer f l = match l with 
    | [] -> false
    | x :: xs -> let y = p x in
                 if y then 
                 let ys = filter f xs in x :: ys 
                 else ys  
