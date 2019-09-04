exception TestExp
type pair = Pair of int * int 


type pairList = 
             E 
         | L of pair
         | LCons of pair * pairList
        

(*  
let pair_to_string pr = 
        match pr with   
        | Pair (x,y) -> " { "^(string_of_int x)^" ,"^(string_of_int y)^" } "  


let rec pairlist_to_string plist = match plist with 
                E -> " epsilon "  
                | L p -> (" "^(pair_to_string p)^" ")
                | LCons (p1, pl) -> (" "^(pair_to_string p1)^" ; "^(pairlist_to_string pl)^" ") 
 *)


let raise ex = 
  E

        
(* flatten : [(a,b); (C,d); (e,f)] -> [a;b;c;d;e;f]*)
(*flatten : pl -> {l | Rmem (l) = Rpmem(pl) /\ Rord (l) = Rfla (pl)}*)

(* let rec flatten pl = 
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
 *)
 
let rec parseABStar a b src = 
    match src with 
        [] -> E  
        | a :: xs1 -> (match xs1 with 
                     [] -> raise TestExp
                    | b :: xs2 ->   
                                    let x_x_pair = Pair (a,b) in 
                                    let fstpair = L x_x_pair  in 
                                    let lptail = parseABStar a b xs2 in 
                                    let res = LCons (x_x_pair, lptail) in 
                                        res
                    )
        | x :: xs -> raise TestExp
(*  let rec parseABStar a b src =  
    match src with 
        [] -> E  
        | a :: xs1 -> (match xs1 with 
                     [] -> raise TestExp
                    | b :: xs2 -> let x_x_pair = Pair (a,b) in 
                                  (match xs2 with 

                                    [] -> (*change this to E*)
                                            let eml = E in 
                                            let res1 = LCons (x_x_pair, eml) in 
                                        res1
                                    | a :: xs3 -> 
                                        let em = [] in 
                                        let resl =(match xs3 with 
                                        [] -> raise TestExp
                                        | b :: xs4 -> 
                                            let x_x_pair' = Pair (a,b) in 
                                            let sndpair = L x_x_pair'  in 
                                            let lptail = parseABStar a b xs4 in 
                                            let res = LCons (x_x_pair, sndpair) in 
                                            res
                                        )
                                        in
                                        resl 

                                    | x :: xs3 -> raise TestExp    
                                    )
                   | x :: xs2 -> raise TestExp                 
                     )                    
        | x :: xs1 -> raise TestExp                                
 
 *)
(* 
let () = 
      
        let src = [1;2;1;2;2;1] in 
       
 
        let parsedList  = parseABStar 1 2 src  in 

        let () = Printf.printf "%s" (pairlist_to_string parsedList) in 


        () 


 *) 
