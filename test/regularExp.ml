module Make = struct 

  type rExpr = int list

  (* get first element of the list *)
  (*get_first_element : l -> {v | Rhd (l) = {(v)}*)    
  let get_first_element l = List.hd l
  
  (* Converts int to string *)
  let int_to_string i = string_of_int i

  (* Concats two integers *)
  let concat_int i i' = int_of_string ((string_of_int i) ^ (string_of_int i'))

  (* concat the elements at each index and gives a regualar expression with concatenated integers. 
     ("a", "b") ++ ("c", "d") ==> ("ac", "bd") *)
  let rec regConcat l l' = match l with
      | [] -> l' 
      | x :: xs -> match l' with 
                   | [] -> l 
                   | x' :: xs' -> concat_int x x' :: regConcat xs xs'

  (* alteration R | S denotes the set union of sets described by R and S 
   ("a", "b") | ("a", "d") ==> ("a", "b", "d") *)
  let rec regAlteration l l' = match l with 
      | [] -> l' 
      | x :: xs -> match l' with 
                    | [] -> l 
                    | x' :: xs' -> if x = x' then x :: regAlteration xs xs' else x :: (regAlteration xs l')
  
  (* extracts all the list of an integer in a list *)
  let digits n =
    let rec loop n acc =
        if n = 0 then acc
        else loop (n/10) (n mod 10::acc) in
    match n with
    | 0 -> [0]
    | _ -> loop n []

  (* returns last element of the list *)
  let rec last_element l = match l with 
       | [] -> failwith "List is empty"
       | [x] -> x
       | x :: xs -> last_element xs

  let check_hd e l = match l with 
    | [] -> false 
    | x :: xs -> if x = e then true else false

  let rec get_next_element e e' l = 
  match l with 
    | [] -> false
    | [x] -> false
    | x :: xs -> if x = e && check_hd e' xs 
                 then true 
                 else (get_next_element e e' xs)

  let rec get_next_next_element e e' l = 
  match l with 
    | [] -> false
    | x :: xs -> if x = e && (check_hd e' (List.tl xs))
                 then true
                 else get_next_next_element e e' xs 

 (* Anchor ^ which matches any integer starting with the argument provided 
     ^The returns the string that starts with The *)
  let rec anchor_start e l = match l with 
      | [] -> []
      | x :: xs -> if (check_hd e (digits x)) 
                   then x :: anchor_start e xs
                   else anchor_start e xs

(* Anchor $ which matches any integer ending with the argument provide
    The$ returns the string that ends with The *)
  let rec anchor_end e l = match l with 
      | [] -> []
      | x :: xs -> if (last_element (digits x)) = e 
                   then x :: anchor_end e xs
                   else anchor_end e xs

(* Anchor ^$ which matches any integer starting and ending with the arguments provide
    ^The end$ returns the string that starts with The and ends with end*)
  let rec anchor_start_end e e' l = match l with 
      | [] -> []
      | x :: xs -> if ((check_hd e (digits x)) && (last_element (digits x)) = e')
                   then x :: anchor_start_end e e' xs
                   else anchor_start_end e e' xs

(* anchor_exact returns any integer that has digit provides as argument in it *)
let rec anchor_exact e l = match l with 
  | [] -> []
  | x :: xs -> if (List.mem e (digits x)) 
               then x :: anchor_exact e xs 
               else anchor_exact e xs

(* or returns any integer that has a digit (a) followed by digit (c or d) *)
let rec reg_or e e' e'' l = match l with 
 | [] -> []
 | x :: xs -> if (get_next_element e e' (digits x)) || (get_next_element e e'' (digits x)) 
              then x :: reg_or e e' e'' xs 
              else reg_or e e' e'' xs

 (* star returns the integer that has a digit (a) followed by zero or more digit (b) *)
  let rec reg_star e e' l = match l with 
  | [] -> []
  | x :: xs -> if (get_next_element e e' (digits x)) || (List.hd (digits x)) = e 
               then x :: reg_star e e' xs 
               else reg_star e e' xs 

 (* add returns the integer that has a digit (a) followed by one or more digit (b) *)
  let rec reg_add e e' l = match l with 
  | [] -> []
  | x :: xs -> if (get_next_element e e' (digits x) )
               then x :: reg_add e e' xs 
               else reg_add e e' xs 

(* zero_one returns the integer that has a digit (a) followed by zero or one digit (b) *)
  let rec reg_zero_one e e' l = match l with 
  | [] -> []
  | x :: xs -> if ((get_next_element e e' (digits x)) && (not (get_next_next_element e e' (digits x)))) ||
               not (get_next_element e e' (digits x))
               then x :: reg_zero_one e e' xs 
               else reg_zero_one e e' xs 

(* print list *)    
  let rec print_rExp l = match l with 
   | [] -> ()
   | e::l -> print_int e ; print_string " " ; print_rExp l

  end 



