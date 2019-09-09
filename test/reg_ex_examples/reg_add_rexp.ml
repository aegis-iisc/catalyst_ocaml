 let eq x1 y1 = true

 exception TestExp

 let raise ex = 
   		[]

 let and_fun x2 y2 = match x2 with 
     | true -> match y2 with 
               | true -> true 
               | false -> false
     | false -> false

 let check_hd el ls = match ls with 
    | [] -> false  
            
    | x :: xs -> let y = (eq x el) in 
                 if y then 
                  true else false

(*incorrect implementation wrt the specification*)
(*
get_next_element : e1 -> e1' -> l1 -> {b | [b=true] <=> ((Rhd(l1) = {(e1)}) /\ (Rnext_hd(l) = {(e1')}))};
*)
 let rec get_next_element e1 e1' l1 = 
  match l1 with 
    | [] -> false
    | x :: xs -> let y = (eq x e1) in 
                 let z = (check_hd e1' xs) in 
                 let r = (and_fun y z) in 
                 if r
                 then true 
                 else
                 let rs = (get_next_element e1 e1' xs) in rs




 (* add returns the integer that has a digit (a) followed by one or more digit (b) *)
  let rec reg_add e2 e2' l2 = 
              let nel= (get_next_element e2 e2' l2) in 
               if nel
               then l2
               else raise TestExp
 

 (* Incorrect implementation*)
(*   let rec reg_add e2 e2' l2 = 
              let nel= (get_next_element e2 e2' l2) in 
               if nel
               then l2
               else let em = [] in 
                em 
 *) 