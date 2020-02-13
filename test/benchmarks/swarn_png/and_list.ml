type pair = Pair of int list * int list 


let rec and_list l = match l with 
(Pair(l1, l2)) -> match l1 with 
                  | [] -> []
                  | x :: xs -> match l2 with 
                                 | [] -> []
                                 | y :: ys -> let r = and_bit (Pair_int(x, y)) in 
                                              let rs = and_list (Pair(xs, ys)) in 
                                              r :: rs 

(*let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl*)