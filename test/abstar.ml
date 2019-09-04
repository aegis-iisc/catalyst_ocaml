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

let isABPair p = 
        match p with 
            | Pair (x, y) -> if ( x != 1)
                                then  
                                    false 
                              else       
                                if (y = 2) 
                                    then true 
                                else false
let rec isABList pl = 
        match pl with
         E -> true 
         | L p -> if isABPair p then true else false
         | LCons (p, pl) -> let pred1 = isABPair p in 
                            let pred2 = isABList pl in  
                            if pred1 = false 
                                then false 
                            else 
                                pred2

let rec concat l1 l2 = 
  match l1 with 
        [] -> l2 
        | x :: xs -> let temp = concat xs l2 in 
                x :: temp  



        
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

let rec parseABStar src =  
    match src with 
        [] -> E 
        | x :: xs -> match xs with 
                    | [] -> let failingPair = Pair (-1, -1) in 
                            let fres = L failingPair in 
                            fres
                    | x' :: xs' -> let x_x_pair = Pair (x,x') in 
                                    let fstpair = L x_x_pair  in 
                                    if isABList fstpair then
                                        let lptail = parseABStar xs' in 
                                        let res = LCons (x_x_pair, lptail) in 
                                        res
                                    else 
                                        let failingPair = Pair (-1, -1) in 
                                        let fres = L failingPair in 
                                        fres


 

(* 
let () = 
      
        let src = [1;2;1;2;1;3] in 
       
 
        let parsedList  = parseABStar src  in 

        let () = Printf.printf "%s" (pairlist_to_string parsedList) in 


        () 

 *)
 