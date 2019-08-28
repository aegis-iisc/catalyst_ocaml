type btree = 
        Leaf of int
        | Node of btree * int * btree 

let rec reversetree t = 
  match t with
    Leaf el -> t 
  | Node (lt,n,rt) ->
          Node (rt, n ,lt)