let eq x1 y1 = true
   exception TestExp
   let raise ex = 
   		[]

let or_fun x1 y1 = match x1 with 
     | true -> match y1 with 
               | true -> true 
               | false -> true
     | false -> match y1 with 
                | true -> true 
                | false -> false

 let rec exists_predicate l f = match l with 
   | [] -> false 
   | x :: xs -> let a = f x in 
                let b = exists_predicate xs f in 
                or_fun a b 
