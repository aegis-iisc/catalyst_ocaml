 exception TestExp
type pair = Pair of int * int 


type pairList = 
             E 
         | L of pair
         | LCons of pair * pairList
        

 
let pair_to_string pr = 
        match pr with   
        | Pair (x,y) -> " { "^(string_of_int x)^" ,"^(string_of_int y)^" } "  


let rec pairlist_to_string plist = match plist with 
                E -> " epsilon "  
                | L p -> (" "^(pair_to_string p)^" ")
                | LCons (p1, pl) -> (" "^(pair_to_string p1)^" ; "^(pairlist_to_string pl)^" ") 






(*A correct version of the parser*)
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
 
 



let () = 
      
  let src = [1;2;1;2;1] in 
       
 
        let parsedList  = parseOneTwoStar src  in 

        let () = Printf.printf "%s" (pairlist_to_string parsedList) in 


        () 




