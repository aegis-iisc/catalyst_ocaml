type t = T of string 
(*
let rec concat l1 l2 = 
        match (l1, l2) with 
        ([], _) -> l2 
        | (_, []) -> l2 
        | (x1::xs1,_ ) -> x1 :: concat xs1 l2
*)
(*let ()= 
        let ls1 = [2;3;4] in 
        let ls2 = [5;6;7] in 
        let lsc = concat ls1 ls2 in 
        let concat_str = List.fold_left (fun accm litem -> accm^(string_of_int litem)) " " lsc in 
        Printf.printf "%s" concat_str

*)

(*let foo x =  x +1*)
(*foo 2 in 
*)
(*let bar = fun (x , w) ->
        let z  = 2 in 
        let y = 3 in 
        y + z + x + w*)
