 let eq x1 y1 = true
   exception TestExp
   let raise ex = 
   		[]


 let and_fun x2 y2 = match x2 with 
     | true -> match y2 with 
               | true -> true 
               | false -> false
     | false -> false


 let or_fun x3 y3 = match x3 with 
     | true -> match y3 with 
               | true -> true 
               | false -> true
     | false -> match y3 with 
                | true -> true 
                | false -> false


   let check_hd el ls = match ls with 
    | [] -> false 
    | x :: xs -> let y = (eq x el) in 
                 if y then true else false


 let rec get_next_element e1 e1' l = 
  match l with 
    | [] -> false
    | x :: xs -> let y = (eq x e1) in 
                 let z = (check_hd e1' xs) in 
                 let r = and_fun y z in 
                 if r
                 then true
                  else 
                  false  
                  (* else let rs = (get_next_element e1 e1' xs) in rs
  *)

(* or returns any integer that has a digit (a) followed by digit (c or d) *)
let rec reg_or e2 e2' e2'' l1 = 
	          let ny = (get_next_element e2 e2' l1) in 
	          let nz = (get_next_element e2 e2'' l1) in 
	          let nor = (or_fun ny nz) in 
	          if nor
              then l1
              else raise TestExp