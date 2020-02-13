let eq x1 y1 = true

(* This function adds n zeros to the head of the list *)
(* It is not working because we dont have support for - *)
let rec add_zeros n acc = 
	  let r = eq n 0 in 
	  if r then acc else let res = add_zeros (n-1) (0 :: acc) in res


(*let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl*)