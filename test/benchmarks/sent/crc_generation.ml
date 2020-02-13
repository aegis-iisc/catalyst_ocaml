exception TestExp

type bit = Zero | One 
type pair = Pair of bit * bit 
(*list of pair of bits, the xor function is defined using a map over such a pair_list*)
type list_pair =
      E 
      | L of pair 
      | LCons of pair * list_pair


(*enumeration of the XOR logic between two bits*)
let xor_b p = 
  match p with 
  Pair (b1, b2) ->
    match b1 with 
      | Zero -> (match b2 with 
          Zero -> 0  
          | One -> 1)
      | One -> (match b2 with 
          Zero -> 1 
          | One -> 0)    




(*map over a list_pair*) 
let rec map f ls = 
  match ls with 
    | E -> []
    | L p -> let f_p = f p in 
        let empty = [] in 
          let res = f_p :: empty in 
          res  
    | LCons (p, pl) ->
           let f_p = f p in
           let  f_pl = map f pl in    
           let res = f_p :: f_pl in 
                res

(*xor over a list of pairs, creates a output list of bitwise xor of each pair
e.g [(1,1);(0,1)] -> [0;1]*)
let xor pl = 
  let res = map xor_b pl in 
  res



let rec zip l1 l2 = 
  match l1 with 
    | [] -> (match l2 with 
           | [] -> E
           | x :: xs -> raise TestExp)
   | y :: ys -> (match l2 with 
           | [] -> raise TestExp 
           | x :: xs -> let result = zip ys xs in 
                        let p = Pair (y,x) in 
                        let res = LCons ( p , result) in 
                         res)
(*takes a list_pair term created by zipping the data and the generator *)
let generate_crc pl  =
  let res = xor pl in 
	res 


(* let () = 
    let g = [Zero;Zero;Zero;One;One;One;Zero] in 
    let l = [One;One;Zero;Zero;One;Zero;One] in 

    let zipped = zip l g in   
    let crc  =  generate zipped in 
    ()  
 