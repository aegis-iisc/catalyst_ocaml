type 'a tree = Leaf of 'a 
                | Node of 'a tree * 'a * 'a tree 

let rec concat l1 l2 = match l1 with 
    [] -> l2 
  | x :: xs -> let temp1 = concat xs l2 in 
                x :: temp1 

let preOrder t = match t with
    Leaf x -> [x]
  | Node z ->
           let  val (l,x,r) = z
     in
       concat (concat (preOrder l) [x]) (preOrder r)
let  postOrder t = match t with
    Leaf x -> [x] 
  | Node (l,x,r) -> concat (concat (postOrder l) (postOrder r)) [x]
                                                                                              

