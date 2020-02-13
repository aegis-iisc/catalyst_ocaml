let rec last_element l = match l with 
| [] -> []
| x :: [] -> [x] 
| x :: xl -> last_element xl

(*let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl*)


(*let rec last_element l = match l with 
 | [] -> []
 | x :: xl -> let next = last_element xl in 
              if xl = [] 
              then (x :: nil)
              else next*)


