 let eq x1 y1 = true

 exception TestExp

 let raise ex = 
      []

 let and_fun x1 y1 = match x1 with 
     | true -> match y1 with 
               | true -> true 
               | false -> false
     | false -> false


 let or_fun x1 y1 = match x1 with 
     | true -> match y1 with 
               | true -> true 
               | false -> true
     | false -> match y1 with 
                | true -> true 
                | false -> false

 let not_fun x1 = match x1 with 
     | true -> false 
     | false -> true 

 let get_tail l = match l with 
     | [] -> []
     | x :: xs -> xs

 let check_hd e l = match l with 
    | [] -> false 
    | x :: xs -> let y = (eq x e) in 
                 if y then true else false


 let rec get_next_element e e' l = 
  match l with 
    | [] -> false
    | x :: xs -> let y = (eq x e) in 
                 let z = (check_hd e' xs) in 
                 let r = and_fun y z in 
                 if r
                 then true 
                 else let rs = (get_next_element e e' xs) in rs

let rec get_next_next_element e e' l = 
  match l with 
    | [] -> false
    | x :: xs -> let y = (eq x e) in 
                 let t = (get_tail xs) in 
                 let z = (check_hd e' t) in
                 let r = and_fun y z in 
                 if r
                 then true
                 else let rs = get_next_next_element e e' xs in rs

(* zero_one returns the integer that has a digit (a) followed by zero or one digit (b) *)
let rec reg_zero_one e e' l = 
               let x = (get_next_element e e' l) in 
               let y = (get_next_next_element e e' l) in 
               let z = not_fun y in 
               let ra = and_fun x z in 
               let ro = not_fun x in 
               let r = or_fun ra ro in 
               if r 
               then l
               else raise TestExp