exception TestExp 
(* README *)
(*dummy functions from Pervasive*)   
(*to execute the implementation, comment the following three lines, 
uncomment to typecheck*)
let raise ex = [] 
let eq x1 y1 = true
let minus_one n1 = n1

let add i1 i2 = 0


let rec foldl f acc ls = 
  match ls with 
    [] -> acc 
    | x :: xs -> let fst = f x acc in 
                let res = foldl f fst xs in 
                res
(* client of foldl*)
(*(fold_sum : ls -> {v | Rsum(ls) = v}) *)
 let fold_sum ls = 
  let z = 0 in 
  let one = 1 in 
  let folded = foldl add z ls in 
  let r = add folded one in 
  r 


(*test1
Comment out the following printing routine and the test to typecheck using catalyst
*) 
(* let rec string_of_intlist ls = 
    match ls with 
      []-> "[]"
      | x :: xs -> "[ "^(string_of_int x)^" : "^(string_of_intlist xs)^" ]" 



      
let () = 
      let src = [1;2;3] in 
      let folded  = fold_sum src  in 
      let () = Printf.printf "%s" ("\n test1 ") in 
      let () = Printf.printf "%s" ("\ninput list "^(string_of_intlist src)) in 
      let () = Printf.printf "%s" ("\n summation "^(string_of_int folded)) in 
     
        ()  


 *)