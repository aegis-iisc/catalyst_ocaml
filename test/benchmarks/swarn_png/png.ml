(* Constraint: The integers in the list must be 0s or 1s *)
type pair_int = Pair_int of int * int 
type pair = Pair of int list * int list 


let rec xor (Pair(l1,l2)) = match l1 with 
 | [] -> []
 | x :: xs -> match l2 with 
              | [] -> []
              | y :: ys -> if x = y then 0 :: xor (Pair(xs,ys)) else 1 :: xor (Pair(xs,ys))


let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl

(* We have the specification written by Ashish *)
let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                temp2

(* Specification:
   (1) Last bit should be zero 
   (2) Rest of the bits should be same as input *)
(* Wrote the final spec *)
let shift_left_one_bit l2 = match l2 with 
| [] -> []
| x :: xs -> concat xs [0]



let left_shift_followed_xor (Pair(l1,l2)) = let r = shift_left_one_bit l1 in 
                                            let result = xor (Pair(r, l2)) in 
                                            result

(* 
(* Specification will be just concerened with the msb bit *)
(* r : c : {v | v = true <=> Rhd{c} = {(1)}} *)
let check_most_significant_bit_set c = 
	let len = list_length c in 
	let r = add_zeros len [] in 
	let r' = set_msb r in 
	let r'' = (and_list (Pair(c, r'))) in 
	let r''' = check_if_zero_or_not r'' in 
	if r''' then false else true  *)

let check_most_significant_bit_set c = 
  match c with 
  | [] -> false 
  | x :: xs -> if (x=1) then true else false

(* g is one bit more than c *)
let compute_crc_one_step c g = if check_most_significant_bit_set c then left_shift_followed_xor (Pair(c,g)) else shift_left_one_bit c


(* here g is the generator which is fixed *)
(* I have tested it for 8 bits where g is taken as [0;0;0;1;1;1;0;1] *)
(* here n is 7 as the counting starts from 0 and goes to 7 total 8 bits *)
let rec generate_crc n l g = 
	if n>0 then 
	let res1 = compute_crc_one_step l g in 
	let res = generate_crc (n-1) res1 g in 
	res 
    else compute_crc_one_step l g

let compute_png n 
l g = 
	let r = (generate_crc n l g) in 
	let result = concat l r in 
	result 


let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl

 (*crc = [1;1;0;0;0;0;1;0] = 0xC2
 g = [0;0;0;1;1;1;0;1] = 0x1D*)

let () = 
    let g = [0;0;0;1;1;1;0;1] in 
    let l = [1;0;1;0;0;1;0;1] in 

      
    let png  = compute_png 7 l g in 
        let () = Printf.printf "%s" ("\n data ") in 
      let () = (print_list l) in 
          let () = Printf.printf "%s" ("\n full ") in 
  
    let () = (print_list png) in 
     
        ()  
