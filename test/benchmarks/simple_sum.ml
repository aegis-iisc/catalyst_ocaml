exception TestExp 
(* README *)
(*dummy functions from Pervasive*)   
(*to execute the implementation, comment the following three lines, 
uncomment to typecheck*)
let raise ex = [] 
let eq x1 y1 = true
let minus_one n1 = n1
let add i1 i2 = 0



(*output is sum of the list*)
 let rec simple_sum ls = 
  match ls with 
    [] -> 0 
    | x :: xs -> let sum = simple_sum xs in 
                  let res = add x sum in 
                  res
 
(*test1
Comment out the following printing routine and the test to typecheck using catalyst
*) 
(* let rec string_of_intlist ls = 
    match ls with 
      []-> "[]"
      | x :: xs -> "[ "^(string_of_int x)^" : "^(string_of_intlist xs)^" ]" 



      
let () = 
      let src = [1;2;3] in 
      let folded  = simple_sum src  in 
      let () = Printf.printf "%s" ("\n test1 ") in 
      let () = Printf.printf "%s" ("\ninput list "^(string_of_intlist src)) in 
      let () = Printf.printf "%s" ("\n summation "^(string_of_int folded)) in 
     
        ()  

 *)
