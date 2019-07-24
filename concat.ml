let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x::xs -> x::(concat xs l2)
