type tree = Leaf
        | Node of tree * int * tree 



let insert el t1 =
       let leafnode = Leaf in  
       Node (leafnode, el, t1)  



let rec concat l1 l2 = 
	match l1 with 
    [] -> l2 
  | x :: xs -> let temp1 = concat xs l2 in 
                let temp2 = x :: temp1  in 
                temp2
