relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;



relation Rfst  (Pair (x,y)) = {(x)};
relation Rsnd (Pair (x,y)) = {(y)};


relation Rplmem (E) = {()} 
				| (L p) =  Rfst (p) U Rsnd (p) 
				| (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rplmem (pl));

relation Rpairs (E) = {()} 
				| (L p) =  (Rfst (p) X Rsnd (p)) 
				| (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rpairs (pl));



relation Rfla (E) = {()}
				| (L p) = (Rfst (p) X Rsnd (p))
				|  (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl)) U Rfla (pl) ;


concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\
               		Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};



flatten : plist -> { lf | Rmem (lf) = Rplmem(plist) /\ Robs (lf) = Rfla (plist)};





