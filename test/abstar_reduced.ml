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
 
let eq x1 y1 =
    true  

let raise ex = 
  E

let isABPair p = 
        match p with 
            | Pair (x, y) ->    let value1 = 1 in 
                                let xeq1 = eq x value1 in 
                                let res = 
                                    if (xeq1)
                                        then 
                                        let value2     = 2 in 
                                        let yeq2 = eq y value2 in  
                                        if ( yeq2)
                                            then true 
                                        else 
                                            false  
                                else       
                                    false
                                in 
     
                                res    

(*    
let rec isABList pl = 
        match pl with
         E -> true 
         | L p ->  let pres = isABPair p in 
                   pres   
         | LCons (p, pl) ->  let pres = isABPair p in 
                             let res= 
                                if (pres) then 
                                        let plres = isABList pl in 
                                            if (plres) then true 
                                                else false  
                                else 
                                    false                     
                              in 
                             res       
     
let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x::xs ->  let temp1 = concat xs l2 in
                let temp2 = x::temp1 in 
                        temp2
 *)

        
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
    match src with 
        [] -> E  
        | x :: xs -> match xs with 
                     [] -> raise TestExp
                    | x' :: xs' -> let x_x_pair = Pair (x,x') in 
                                    let fstpair = L x_x_pair  in 
                                    let testablist = isABList fstpair in 
                                    let respairlist = 
                                    if (testablist) then
                                            let lptail = parseABStar xs' in 
                                            let res = LCons (x_x_pair, lptail) in 
                                        res
                                    else 
                                        raise TestExp                                                          
                                    in 
                                    respairlist    
 *) 

(* 
let () = 
      
        let src = [1;2;1;2;1;3] in 
       
 
        let parsedList  = parseABStar src  in 

        let () = Printf.printf "%s" (pairlist_to_string parsedList) in 


        () 

 *)
 
