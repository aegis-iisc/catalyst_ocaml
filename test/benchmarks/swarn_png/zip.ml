exception TestExp
type bit = Zero | One 
type pair = Pair of bit * bit 
type list_pair =
      E 
      | L of pair 
      | LCons of pair * list_pair


let raise ex = 
   		E

let rec zip l1 l2 = 
	match l1 with 
   	| [] -> match l2 with 
           | [] -> E
           | x :: xs -> raise TestExp
   | y :: ys -> match l2 with 
           | [] -> raise TestExp 
           | x :: xs -> let result = zip ys xs in 
                        let p = Pair (y,x) in 
                        let res = LCons ( p , result) in 
                         res
