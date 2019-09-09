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
 (*A correct parser*)
(* let rec parseOneTwoStar src = 
    match src with 
        [] -> E  
        | 1 :: xs1 -> (match xs1 with 
                     [] -> raise TestExp
                    | 2 :: xs2 ->   let const1 = 1 in 
                                    let const2 = 2 in 
                                    let x_x_pair = Pair (const1, const2) in 
                                    let fstpair = L x_x_pair  in 
                                    let lptail = parseOneTwoStar xs2 in 
                                    let res = LCons (x_x_pair, lptail) in 
                                        res (*Change this to lptail to get a failing typechecking*)
                    )
        | x1 :: xs1 -> raise TestExp
 *)

 (*An incorrect Parser which will typecheck with relational properties
 but fails to typecheck when using Length properties
parseOneTwoStar src 
    [] -> [()]
    | [1;2] -> [(1,2)]
    | [1;2;1;2;{1,2}] -> [(1,2);(1,2)]

*)  

(*  let rec parseOneTwoStar  src =  
    let const1 =1 in 
    let const2 =2 in 
    match src with 
        [] -> E  
        | 1 :: xs1 -> (match xs1 with 
                     [] -> raise TestExp
                    | 2 :: xs2 -> let x_x_pair = Pair (const1,const2) in 
                                  (match xs2 with 

                                    [] -> (*change this to E*)
                                            let eml = E in 
                                            let res1 = LCons (x_x_pair, eml) in 
                                        res1
                                    | 1 :: xs3 -> 
                                        (match xs3 with 
                                            [] -> raise TestExp
                                            | 2 :: xs4 -> 
                                                ( match xs4 with 
                                                 [] ->let x_x_pair' = Pair (const1,const2) in 
                                                    let sndpair = L x_x_pair'  in 
                                                    (* let lptail = parseABStar xs4 in 
                                                     *)let res = LCons (x_x_pair, sndpair) in 
                                                    res
     
                                                | 1 :: xs5 -> 
                                                    (match xs5 with 
                                                    [] -> 
                                                        let x_x_pair' = Pair (const1,const2) in 
                                                        let sndpair = L x_x_pair'  in 
                                                       (*  let lptail = parseOneTwoStar xs4 in 
                                                        *) let res = LCons (x_x_pair, sndpair) in 
                                                         (*retrun sndpair to see the typechecking failing*)
                                                        res
                                                    | x6:: xs6 -> raise TestExp     
                                                    )
                                                | 2 :: xs5 ->
                                                    (match xs5 with 
                                                    [] -> 
                                                        let x_x_pair' = Pair (const1,const2) in 
                                                        let sndpair = L x_x_pair'  in 
                                                        (* let lptail = parseABStar xs4 in 
                                                         *)let res = LCons (x_x_pair, sndpair) in 
                                                        res
                                                    | x6:: xs6 -> raise TestExp     
                                                    )  

                                                |x5 :: xs5 -> raise TestExp 

                                                )
                                            | x4 :: xs4 -> raise TestExp    
                                         )

                                    | x3 :: xs3 -> raise TestExp    
                                    )
                   | x2 :: xs2 -> raise TestExp                 
                     )                    
        | x1 :: xs1 -> raise TestExp                                
  *)
(*A correct version of the parser*)
  let rec parseOneTwoStar  src =  
    let const1 =1 in 
    let const2 =2 in 
    match src with 
        [] -> E  
        | 1 :: xs1 -> (match xs1 with 
                     [] -> raise TestExp
                    | 2 :: xs2 -> let x_x_pair = Pair (const1,const2) in 
                                  (match xs2 with 

                                    [] -> (*change this to E*)
                                            let eml = E in 
                                            let res1 = LCons (x_x_pair, eml) in 
                                        res1
                                    | 1 :: xs3 -> 
                                        (match xs3 with 
                                            [] -> raise TestExp
                                            | 2 :: xs4 -> 
                                                ( match xs4 with 
                                                 [] ->let x_x_pair' = Pair (const1,const2) in 
                                                    let sndpair = L x_x_pair'  in 
                                                    (* let lptail = parseOneTwoStar xs4 in 
                                                     *)let res = LCons (x_x_pair, sndpair) in 
                                                    res
     
                                                
                                                | x5::xs5 -> raise TestExp 

                                                )
                                            | x4 :: xs4 -> raise TestExp    
                                         )

                                    | x3 :: xs3 -> raise TestExp    
                                    )
                   | x2 :: xs2 -> raise TestExp                 
                     )                    
        | x1 :: xs1 -> raise TestExp                                
  


(* 

let () = 
      
        let src = [1;2;1;2;1] in 
       
 
        let parsedList  = parseABStar src  in 

        let () = Printf.printf "%s" (pairlist_to_string parsedList) in 


        () 

 *)
 
