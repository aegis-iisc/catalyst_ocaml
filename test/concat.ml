let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x::xs -> let temp1 = concat xs l2 in
              x :: temp1
