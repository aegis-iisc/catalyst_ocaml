type tree = Leaf of int
                | Node of tree * int * tree 

let rec concat l1 l2 = match l1 with 
    [] -> l2 
  | x :: xs -> let temp1 = concat xs l2 in 
                x :: temp1 

let rec preOrder t = match t with
    Leaf x -> [x]
  | Node (l,n,r) ->
        let preR = preOrder r in 
        let preL = preOrder l in 
        let nlist = [n] in          
        let prefixc=  concat (preL) nlist in 
        let res = concat prefixc preR in 
        res

(*let rec  postOrder t = match t with
    Leaf x -> [x] 
  | Node (l,n,r) -> concat (concat (postOrder l) (postOrder r)) [n]
  *)                                                                                            

