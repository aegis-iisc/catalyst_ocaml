exception Ocatalyst
let is_non_empty l = 
        match l with 
        [] -> false
      | x::xs -> true 


let raise ex = 
  -1

let head ls = 
        let empty_test = is_non_empty ls in 
        if (empty_test) 
                then 
                  (let res = match ls with 
                        | x :: xs -> x 
                  in 
                  res)
                else 
                  raise Ocatalyst
(* 
let () = 
        let l1 = [1;2;3] in 
        let l2 = [] in 

        let h1 = head l1 in 
        let h2 = head l2 in 

        let () = Printf.printf "%s" ("\n Head of list "^(string_of_int h1)) in 
         let () = Printf.printf "%s" ("\n Head of list "^(string_of_int h2)) in
        ()
 *)
                         
