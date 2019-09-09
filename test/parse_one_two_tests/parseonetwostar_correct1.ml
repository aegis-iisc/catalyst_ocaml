exception TestExp


type pair = Pair of int * int 


type pairList = 
             E 
         | L of pair
         | LCons of pair * pairList
        


let raise ex = 
  E

 (*A correct parser*)
 let rec parseOneTwoStar src = 
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


