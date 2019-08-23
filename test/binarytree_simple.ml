type btree = 
        Leaf 
  |     Node of btree * int * btree 
           

let insert x s = Node (Leaf , x , s)


