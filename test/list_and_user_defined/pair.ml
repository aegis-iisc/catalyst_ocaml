type pair = Pair of int * int 


type pairList = 
             E 
         | L of pair
         | LCons of pair * pairList
        

(* let pair_to_string pr = 
        match pr with   
        | Pair (x,y) -> " { "^(string_of_int x)^" ,"^(string_of_int y)^" } "  


let rec pairlist_to_string plist = match plist with 
                E -> " epsilon "  
                | L p -> (" "^(pair_to_string p)^" ")
                | LCons (p1, pl) -> (" "^(pair_to_string p1)^" ; "^(pairlist_to_string pl)^" ") 

 *)
let rec concat l1 l2 = 
  match l1 with 
        [] -> l2 
  | x :: xs -> let temp = concat xs l2 in 
                x :: temp  

        
(* flatten : [(a,b); (C,d); (e,f)] -> [a;b;c;d;e;f]*)
(*flatten : pl -> {l | Rmem (l) = Rpmem(pl) /\ Rord (l) = Rfla (pl)}*)

let rec flatten pl = 
        match pl with 
        E -> [] 
        | L p -> (match p with 
                        | Pair (a, b) -> 
                            let em = [] in 
                            let bl = b :: em in
                            let listp = a :: bl in 
                            listp 


                        )
        | LCons (p1 , pl') ->   let spl = L p1 in  
                                let f1 = flatten spl in 
                                let f2 = flatten pl' in 
                                let res = concat f1 f2 in 
                                res




(* 

let () = 
        let p1 = Pair (1,2) in 
        let p2 = Pair (3,4) in 
        let p3 = Pair (5,6) in 
        
        let p3L = L (p3) in 
        let  p2p3L = LCons (p2, p3L) in 

        let pl = LCons (p1, p2p3L) in 

        let () = Printf.printf "%s" (pairlist_to_string pl) in 

        let flattened_pl = flatten pl  in 

        let flattened_str = (List.fold_left (fun accstr item -> 
                                                    (accstr^"; "^(string_of_int item)^" ") ) " [ " flattened_pl)^" ] " in 
        let () = Printf.printf "%s" flattened_str in
        () 


 *)