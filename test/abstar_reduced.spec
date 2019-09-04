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

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)} };

isABPair : p -> {vp | [vp=true] => ( (Rfst (p) = {(1)}) /\ ( Rsnd (p) = {(2)}) ) \/ ( (Rfst (p) = {()}) /\ ( Rsnd (p) = {()}))};
isABList : pl -> {v2 | [v2=true] =>  (Rpairs (pl) = ({(1)} X {(2)})) \/ (Rpairs (pl) = {()})}; 

concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\
               		Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};


parseABStar : src  ->  {v |  ( (Rpairs (v) = ({(1)} X {(2)})) \/ (Rpairs (v) = {()}) ) /\ Robs(src) = Rfla(v)};

