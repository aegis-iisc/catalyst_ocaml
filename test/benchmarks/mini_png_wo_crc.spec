relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;
relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 
relation Rhdn (nil) = (0) | (cons(x,xs)) = (x);



relation Rlenfst (Pair (l, r)) = Rlen (l);
relation Rlensnd (Pair (l, r)) = Rlen (r);
relation Rmemfst (Pair (l,r)) = Rmem (l);
relation Rmemsnd (Pair (l, r)) = Rmem (r);




assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};
assume minus_one : n1 -> {vn | (vn) = (n1) -- (1)};

assume projl : p0 -> {pl | Rmem (pl) = Rmemfst (p0)/\
						Rlen (pl) = Rlenfst(p0) };

assume projr : p1 -> {pr | Rmem (pr) = Rmemsnd (p1)/\
							Rlen (pr) = Rlensnd(p1) };


concat : l1 -> l2 -> { l |  Rlen (l) = Rlen (l1) + Rlen(l2) /\
							Rmem(l) = Rmem(l1) U Rmem(l2) /\ 
							Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};

parse_chunk : ac -> n -> l1 -> {v1 | Rlenfst(v1) =  Rlen(ac) + (n) /\ 
									 Rlen(l1) =  Rlensnd (v1) + (n) /\
									 Rmemfst(v1) C= (Rmem (ac) U Rmem (l1))};

p_star : acc -> 
				inpc ->
						
						(ac -> n ->  l1 -> 
								{v1 |
									 Rhdn(inpc) = {(n)} /\	
									 Rlenfst(v1) =  Rlen(ac) + (n) /\ 
								 	 Rlen(l1) =  Rlensnd (v1) + (n) /\
							 	 	 Rmemfst(v1) C= (Rmem (ac) U Rmem (l1)) 
								}
						) ->

							{v | Rmemfst(v) C ( Rmem(acc) U Rmem (inpc)) 
								/\ Rmemsnd (v) = {()} 
								/\	Rlensnd (v) = {(0)}
							};

parse_header : hd -> inp ->  {vhd | [vhd=true] <=> Rhd(inp) = {(hd)}};

parse_png : hdr -> 
				{l1 | Rhd(l1) = {(hdr)}} -> 
					{v |   Rmemfst (v) C Rmem (l1) /\
						   Rmemsnd (v) = {()} 
					};


