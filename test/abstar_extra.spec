relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;



relation Rfst  (Pair (x,y)) = {(x)};
relation Rsnd (Pair (x,y)) = {(y)};


relation Rplmem (E) = {()} 
				| (L p) =  (Rfst (p) U Rsnd (p)) 
				| (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rpmem (pl));
relation Rpairs (E) = {()} 
				| (L p =  (Rfst (p) X Rsnd (p)) 
				| (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rpairs pl) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl));



// define the Rfla
relation Rfla (E) = {()}
				| (L p) = (Rfst (p) X Rsnd (p))
				|  (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl)) U Rfla (pl) ;


relation isABList (E) = 

concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\
               		Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};





parseAbStar : src  ->  {v : pairList | Rord(src) = Rfla(v) ∧ IsABList(v)}



************************************************
relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;



relation Rfst  (Pair (x,y)) = {(x)};
relation Rsnd (Pair (x,y)) = {(y)};
relation Rabpair (Pair (x,y)) = 


relation Rplmem (E) = {()} 
				| (L p) =  (Rfst (p) U Rsnd (p)) 
				| (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rpmem (pl));

relation Rpairs (E) = {()} 
				| (L p =  (Rfst (p) X Rsnd (p)) 
				| (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rpairs pl);


relation Rallpairs (E) = {()} 
				| (L p =  (Rfst (p) X Rsnd (p)) 
				| (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rpairs pl) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl));



assume isABPair : p -> {v | [v=true] <=> Rfst (p) = {(a) /\ Rsnd (p) = {b}}};
assume isABList	 : pl -> {v | [v=true] <=> Rpairs (v) = ({(a)} X {(b)}) } 

relation Rfla (E) = {()}
				| (L p) = (Rfst (p) X Rsnd (p))
				|  (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl)) U Rfla (pl) ;


concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\
               		Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};





parseAbStar : src  ->  {v : pairList | Rord(src) = Rfla(v) ∧ isABList(v)}



