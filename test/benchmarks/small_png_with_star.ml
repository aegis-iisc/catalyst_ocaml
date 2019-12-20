exception TestExp 
(* README *)
(*dummy functions from Pervasive*)   
(*to execute the implementation, comment the following three lines, 
uncomment to typecheck*)
let raise ex = [] 
let eq x1 y1 = true
let minus_one n1 = n1


(*to execute the implementation, un-comment the following two lines, 
comment to typecheck*)
(* let eq x1 y1 = x1 = y1
let minus_one n1 = n1 - 1
 *)

type lList = 
    | E 
    | LCons of int list * lList
     

(*Internal function to iterate over a list
  @param n : size of the following input list
  @param l : input list 
  @return : the same list as the input
  @raises : TestExp if n and l's size mismatch
  @property : n -> {l1} -> {v1 | Rlen(v1) = (n) /\ Rlen(l1) = (n) }
                          *)
let rec loop n l = 
  match l with 
    [] -> 
      let z = 0 in 	
      let eq_n_z = eq n z in 
      if eq_n_z then  
        []  

      else 
        raise TestExp
  | x :: xs ->
      let n_min_1 = minus_one n in 
      let res = loop n_min_1 xs in
      let out = x :: res in 
      out 			
(* a simple parsing function
   	@param inp : input list 
   	@return : a parsed list, same as input 
   	@property : inp ->  {v | Rhd(inp) = {()} \/ Rhdn (inp) = Rlentail (v)}
                     *)	

(*one correct implementation*)
(* let parse_png inp f  = 
  match inp with 
    [] -> let temp = [] in 
      temp
  | x1 :: xs1 -> 
      let z1 = 0 in 
      let eq_x1_z1 = eq x1 z1 in  
      if (eq_x1_z1)
      then 
        raise TestExp
      else
        let parsed = f x1 xs1 in 
        let res = x1 :: parsed in 
              res                      
 *)(*Incorrect implementation*)
let parse_png inp f = 
  match inp with 
    [] -> let temp = [] in 
      temp
  | x1 :: xs1 -> 
      let z1 = 0 in 
      let eq_x1_z1 = eq x1 z1 in 	
      if (eq_x1_z1)
      then 
        raise TestExp
      else
        let parsed = f x1 xs1 in 
        parsed



(*test1
Comment out the following printing routine and the test to typecheck using catalyst
*) 
(* let rec string_of_intlist ls = 
    match ls with 
      []-> "[]"
      | x :: xs -> "[ "^(string_of_int x)^" : "^(string_of_intlist xs)^" ]" 

(*

@param src : list header, followed by a data, such that header gives the length of the remaining data*)
      
let test src = 
      
      let parsedList  = parse_png src  in 
      let () = Printf.printf "%s" ("\n test1 ") in 
      let () = Printf.printf "%s" ("\ninput list "^(string_of_intlist src)) in 
      let () = Printf.printf "%s" ("\nparsed list "^(string_of_intlist parsedList)) in 
     
        ()  

 *)