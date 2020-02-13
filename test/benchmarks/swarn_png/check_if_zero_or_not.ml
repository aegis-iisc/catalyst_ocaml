let eq x1 y1 = true

(* Checks if all the elements of the list are zero or not *)
let rec check_if_zero_or_not l = match l with 
  | [] -> true
  | x :: xs -> let res = check_if_zero_or_not xs in 
               let res1 = eq x 0 in 
               if res1 then res else false 