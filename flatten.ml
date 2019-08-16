let rec concat l1 l2 = 
  match l1 with 
        [] -> l2 
  | x :: xs -> let temp = concat xs l2 in 
                x :: temp  

let rec flatten ll = 
	match ll with 
	[] -> []
	| l1 :: llr -> let frem = flattem llr in  
				ler res = concat l1 frem  in 
				res
