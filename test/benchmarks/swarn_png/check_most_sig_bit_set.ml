
let check_most_significant_bit_set c = 
  match c with 
  | [] -> false 
  | x :: xs -> if (x=1) then true else false

