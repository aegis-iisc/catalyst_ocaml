relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;
relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 
primitive relation RId = \z.{(z)};



relation Rlenfst (Pair (l, r)) = Rlen (l)
relation Rlensnd (Pair (l, r)) = Rlen (r)




assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};
assume lt : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)};


parse_chunk : {n | n >= 0} -> {l1 | Rlen(l1) >= n} -> {v |
														Rlenfst (v)) = n /\ 
														Rlensnd (v) = (Rlen(l1) - n)}


parse_crc : l ->  {v : Pair(l1, l2) | Rlen (l) > 0 /\ 
										Rlenfst (v) = 1 /\ 
										Rlensnd (v) = Rlen(l) - 1)}




parse_png : {l1 | (Rhd (l1) = {(n)} /\ n >= 0) \/ 
					(Rhd(l1) = {()}) } -> 
			
			f: ({n1 | (n1 >= 0) /\ (n1 = n)} -> 
				{l2 | Rlen (l2) >= (n1)} -> 
				{v | Rlenfst (v)) = n /\ 
					Rlensnd (v) = (Rlen(l1) - n1)}
			g : lc ->  {v | Rlen (lc) > 0 /\ 
							Rlenfst (v) = 1 /\ 
							Rlensnd (v) = Rlen(lc) - 1)}
 		
 			{v | Rlen (v) = Rlen (l1) /\
				 Rmem (v) = Rmem (l1) /\
				 Robs (v) = Robs (l1) };
