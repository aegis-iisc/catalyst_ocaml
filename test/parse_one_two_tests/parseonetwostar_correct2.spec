relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;
relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 



relation Rfst  (Pair (x,y)) = {(x)};
relation Rsnd (Pair (x,y)) = {(y)};



relation Rplmem (E) = {()} 
				| (L p) =  Rfst (p) U Rsnd (p) 
				| (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rplmem (pl));

relation Rpairs (E) = {()} 
				| (L p) =  (Rfst (p) X Rsnd (p)) 
				| (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rpairs (pl));


relation Rplen  (E) = (0)
				| (L p) = (1)
				| (LCons (p, pl)) = (1) + Rplen(pl);


relation Rfla (E) = {()}
				| (L p) = (Rfst (p) X Rsnd (p))
				|  (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl)) U Rfla (pl) ;



assume raise : ex -> {vex | true};



parseOneTwoStar : src   ->  {v |  ( (Rpairs (v) = ({(1)} X {(2)})) \/ (Rpairs (v) = {()}) ) /\ (Robs(src) = Rfla(v)) /\ (Rmem(src) = Rplmem (v))
																								/\ (Rlen(src) = Rplen(v) + Rplen(v))};


