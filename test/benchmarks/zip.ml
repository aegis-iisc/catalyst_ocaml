type pair = Pair of int * int 

let eq x1 y1 = true
   
exception TestExp
   let raise ex = 
   		[]

type pairList = 
             E 
         | L of pair
         | LCons of pair * pairList

 let rec zip l1 l2 = match l1 with 
   | [] -> match l2 with 
           | [] -> E
           | x :: xs -> raise TestExp
   | y :: ys -> match l2 with 
           | [] -> raise TestExp 
           | x :: xs -> let result = zip ys xs in 
                         LCons (Pair (y,x)) result