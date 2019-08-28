type btree = 
        Leaf of int 
  |     Node of btree * int * btree 
           

let insert x t1 =
        let t2 = Node (t1, x, t1)  in 
        let leafnode = Leaf x in 
        leafnode
       (*let res = Node (leafnode, x, t1)   in 
       res*)



