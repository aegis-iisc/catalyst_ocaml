relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;



relation Rfst (Pair(x, y)) = {(x)};
relation Rsnd (Pair(x, y)) = {(y)};


assume raise : ex -> {vex | true};



relation Rxor (Pair(Zero, Zero)) = {(0)}
			| (Pair(Zero, One)) = {(1)}
			| (Pair(One, Zero)) = {(1)}
			| (Pair(One, One)) = {(0)};


relation Rplmem (E) = {()}
                 | (L p) = (Rfst(p) U Rsnd(p))
                 | (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rplmem (pl));


relation Rmemxor E = {()}
				| (L p) = (Rxor(p)) 
				| (LCons (p, pl))  =  Rxor(p) U Rmemxor(pl);



relation Rfstonly (E) = {()}
                  | (L p) = Rfst(p)
                  | (LCons (p, pl)) = Rfst(p) U Rfstonly(pl);


relation Rsndonly (E) = {()}
                  | (L p) = Rsnd(p)
                  | (LCons (p, pl)) = Rsnd(p) U Rsndonly(pl);



assume concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\ 
										Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};


assume zip : l1 -> l2 -> {l | (Rmem(l1) C= Rplmem(l)) /\ 
                                (Rmem(l2) C= Rplmem(l)) /\ 
                                (Rplen(l) = Rlen(l1)) /\ 
                                (Rplen(l) = Rlen(l2)) /\
                                (Rfstonly(l) C= Rmem(l1)) /\
                                (Rsndonly(l) C= Rmem(l2))};

xor_b : p -> {v | {(v)} = Rxor (p)}; 

map : (p -> {v | {(v)} = Rxor (p)}) -> l1 -> {vout | Rmem (vout) = Rmemxor (l1)};

xor : pl -> {vxor | Rmem(vxor) = Rmemxor (pl)};

generate_crc : pl -> {v | Rmem(v) = Rmemxor(pl)};