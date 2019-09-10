exception TestExp


type pair = Pair of int * int 


type pairList = 
             E 
         | L of pair
         | LCons of pair * pairList
        


let raise ex = 
  E





(*An incorrect Parser which will typecheck with relational properties
 but fails to typecheck when using Length properties
parseOneTwoStar src 
    [] -> [()]
    | [1;2] -> [(1,2)]
    | [1;2;1;2;{1,2}] -> [(1,2);(1,2)]

*)  

  let rec parseOneTwoStar  src =  
    let const1 =1 in 
    let const2 =2 in 
    match src with 
        [] -> E  
        | 1 :: xs1 -> (match xs1 with 
                     [] -> raise TestExp
                    | 2 :: xs2 -> let x_x_pair = Pair (const1,const2) in 
                                  (match xs2 with 

                                    [] -> (*change this to E for typechecking failing*)
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
  
