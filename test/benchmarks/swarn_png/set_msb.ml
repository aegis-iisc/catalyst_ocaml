let eq x1 y1 = true

let set_msb l = match l with 
| [] -> []
| x :: xs -> let r = eq x 1 in 
             if r then l else 1 :: xs  


(*let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl*)