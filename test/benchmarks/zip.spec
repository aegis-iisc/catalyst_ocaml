relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};

relation Robs = Rob*;

relation Roas = Roa*;

relation Rfst (Pair(x, y)) = {(x)};

relation Rsnd (Pair(x, y)) = {(y)};

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)} };

relation Rplmem (E) = {()}
                 | (L p) = (Rfst(p) U Rsnd(p))
                 | (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rplmem (pl));

relation Rpairs (E) = {()} 
				| (L p) =  (Rfst (p) X Rsnd (p)) 
				| (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rpairs (pl));


relation Rplen  (E) = (0)
				| (L p) = (1)
				| (LCons (p, pl)) = (1) + Rplen(pl);

relation Rlen (cons(x, xs)) = ((1) + Rlen(xs)) | nil = (0);


relation Rfla (E) = {()}
				| (L p) = (Rfst (p) X Rsnd (p))
				|  (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl)) U Rfla (pl);

relation Rfstonly (E) = {()}
                  | (L p) = Rfst(p)
                  | (LCons (p, pl)) = Rfst(p) U Rfstonly(pl);


relation Rsndonly (E) = {()}
                  | (L p) = Rsnd(p)
                  | (LCons (p, pl)) = Rsnd(p) U Rsndonly(pl);


relation zip : l1 -> l2 -> {l | (Rmem(l1) C= Rplmem(l)) /\ 
                                (Rmem(l2) C= Rplmem(l)) /\ 
                                (Rplen(l) = Rlen(l1)) /\ 
                                (Rplen(l) = Rlen(l2)) /\
                                (Rfstonly(l) C= Rmem(l1)) /\
                                (Rsndonly(l) C= Rmem(l2))}