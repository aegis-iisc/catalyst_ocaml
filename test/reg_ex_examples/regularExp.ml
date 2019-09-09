module Make = struct 

  type rExpr = int list

  type pair = Pair of int * int

  type pairList = 
             E 
         | L of pair
         | LCons of pair * pairList

  (* get first element of the list *)
  let get_first_element l = List.hd l
  
  (* Converts int to string *)
  let int_to_string i = string_of_int i

  (* Concats two integers *)
  let concat_int i i' = int_of_string ((string_of_int i) ^ (string_of_int i'))

  (* concat the elements at each index and gives a regualar expression with concatenated integers. 
     ("a", "b") ++ ("c", "d") ==> ((a,b), (b,d)) *)
  let rec regConcat l l' = match (l, l') with 
    | [], [] -> E
    | [], _ -> failwith "concatenation not possible"
    | _, [] -> failwith "concatenation not possible"
    | (x :: xs), (y :: ys) -> LCons (Pair (x, y), regConcat xs ys) 

  (* alteration R | S denotes the set union of sets described by R and S 
   ("a", "b") | ("a", "d") ==> ("a", "b", "d") *)
  let rec regAlteration l l' = match l with 
      | [] -> l' 
      | x :: xs -> match l' with 
                    | [] -> l 
                    | x' :: xs' -> if x = x' then x :: regAlteration xs xs' else x :: (regAlteration xs l')

  (* extracts all the digits of an integer in a list *)
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
      | x :: xs -> if (check_hd e l) 
                   then l
                   else failwith "parse error"

(* Anchor $ which matches any integer ending with the argument provide
    The$ returns the string that ends with The *)
  let rec anchor_end e l = match l with 
      | [] -> []
      | x :: xs -> if (last_element l) = e 
                   then l
                   else failwith "parse error"

(* Anchor ^$ which matches any integer starting and ending with the arguments provide
    ^The end$ returns the string that starts with The and ends with end*)
  let rec anchor_start_end e e' l = match l with 
      | [] -> []
      | x :: xs -> if ((check_hd e l) && (last_element l) = e')
                   then l
                   else failwith "parse error"

  let rec compare_each_elm_equality l l' = match (l, l') with 
     | [], [] -> true 
     | [], _ -> false
     | _, [] -> false
     | x :: xs, y :: ys -> if x = y 
                           then compare_each_elm_equality xs ys 
                           else false 

(* anchor_exact returns any integer that has digit provides as argument in it *)
let rec anchor_exact e l = if compare_each_elm_equality e l
                            then l 
                            else failwith "parse error" 

(* or returns any integer that has a digit (a) followed by digit (c or d) *)
let rec reg_or e e' e'' l = if (get_next_element e e' l || get_next_element e e'' l) 
              then l
              else failwith "parse error"
(*
 (* star returns the integer that has a digit (a) followed by zero or more digit (b) *)
  let rec reg_star e e' l = match l with 
  | [] -> []
  | x :: xs -> if (get_next_element e e' (digits x)) || (List.hd (digits x)) = e 
               then x :: reg_star e e' xs 
               else reg_star e e' xs *)

 (* add returns the integer that has a digit (a) followed by one or more digit (b) *)
  let rec reg_add e e' l = if (get_next_element e e' l)
               then l
               else failwith "parse error"

(* zero_one returns the integer that has a digit (a) followed by zero or one digit (b) *)
  let rec reg_zero_one e e' l = if ((get_next_element e e' l) && (not (get_next_next_element e e' l))) ||
               not (get_next_element e e' l)
               then l
               else failwith "parse error"
(* print pair *)
let print_pair p = print_string "("; print_int (fst p); print_string " " ; print_string ","; print_int (snd p) ; print_string ")"

(* print list *)
let print_rExp f l =
  let rec print_elements = function
    | [] -> ()
    | h::t -> f h; print_string ";"; print_elements t
  in
  print_string "[";
  print_elements l;
  print_string "]"

(* prints the Pair type *)
let print_pairType (Pair (p, q)) = print_string "("; print_int p; print_string " "; print_string ","; print_int q; print_string ")" 

(* prints the pairList *)
let rec print_pairList l = match l with
   | E -> print_string " "
   | L p -> print_pairType p 
   | LCons (p, pl) -> print_pairType p ; print_string " " ; print_pairList pl

end 



