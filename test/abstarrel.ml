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
 
(* let rec parseABStar src =  
    let a = 1 in 
    let b = 2 in 
    let c = 3 in 
    match src with 
        [] -> E  
        | a :: xs -> match xs with 
                     [] -> raise TestExp
                    | b :: xs' -> let x_x_pair = Pair (a,b) in 
                                    let fstpair = L x_x_pair  in 
                                    let lptail = parseABStar xs' in 
                                    let res = LCons (x_x_pair, lptail) in 
                                        res

        | x :: xs -> raise TestExp                                
 *)
let parseABStar src =  
    let a = 1 in 
    let b = 2 in 
    match src with 
        [] -> E  
        | a :: xs -> match xs with 
                     [] -> raise TestExp
                    | b :: xs' -> let x_x_pair = Pair (a,b) in 
                                  (match xs' with 
                                    [] -> E
                                    | a :: xs'' -> 
                                        let em = [] in 
                                        (match xs'' with 
                                        [] -> raise TestExp
                                        | b :: em -> 
                                            let x_x_pair' = Pair (a,b) in 
                                            let sndpair = L x_x_pair'  in 
                                            let res = LCons (x_x_pair, sndpair) in 
                                            res )
                                    | x :: xs'' -> raise TestExp    
                                    )
                   | x :: xs' -> raise TestExp                 
                                         
        | x :: xs -> raise TestExp                                


(* 
let () = 
      
        let src = [1;2;1;2;] in 
       
 
        let parsedList  = parseABStar src  in 

        let () = Printf.printf "%s" (pairlist_to_string parsedList) in 


        () 

 *)
 
