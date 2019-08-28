type btree = 
        Leaf of int
        | Node of btree * int * btree 

let rec concat l1 l2 = match l1 with 
    [] -> l2 
  | x :: xs -> let temp1 = concat xs l2 in 
                let temp2 = x :: temp1  in 
                temp2

let rec preOrder tr = 
  match tr with
    Leaf el -> let t0 = [] in 
              let t1 = el :: t0 in 
                t1
  | Node (lt,n,rt) ->
          let preR = preOrder rt in 
           let preL = preOrder lt in
        let em = [] in  
        let nlist = n :: em in          
        let prefixc =  concat (preL) nlist in 
        let res = concat prefixc preR in 
        res
 
