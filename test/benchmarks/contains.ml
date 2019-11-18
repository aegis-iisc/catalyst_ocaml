(* I don't think exists and contains are different if we just consider one element.
   So contains here is like subset *)

 let eq x1 y1 = true
   exception TestExp
   let raise ex = 
   		[]

 let contains l1 l2 = match l1 with 
   | [] -> false 
   | x :: xs = let a = exists x l2 in  
               if a then let b = contains xs l2 in b 
                    else false  
