let rec concat l3 l4 = 
match l3 with
    [] -> l4
  | x::xs -> x::(concat xs l4)
