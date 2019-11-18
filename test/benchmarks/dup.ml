type pair = Pair of int * int 

let eq x1 y1 = true
   
exception TestExp
   let raise ex = 
   		[]

type pairList = 
             E 
         | L of pair
         | LCons of pair * pairList

 let rec dup l1 = match l1 with 
   | [] -> []
   | x :: xs -> let result = dup xs in 
                LCons (Pair(x, x), result)