   let eq x1 y1 = true
   exception TestExp
   let raise ex = 
   		[]

  let rec mem_check e l = match l with 
     | [] -> false 
     | x :: xs -> let r = eq e x in 
                  if r 
                  then true 
                  else let rs = mem_check e xs in rs 

(* anchor_exact returns any integer that has digit provides as argument in it *)
let rec anchor_exact e1 l1 = let r = mem_check e1 l1 in 
                            if r 
                            then l1 
                            else raise TestExp
(*Incorrect implementation*)
(* let rec anchor_exact e1 l1 = let r = mem_check e1 l1 in 
                            if r 
                            then [] (*returning an empty list*) 
                            else raise TestExp
 *)