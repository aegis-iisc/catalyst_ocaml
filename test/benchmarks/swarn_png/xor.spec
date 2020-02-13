relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;



relation Rfst (Pair(x, y)) = {(x)};

relation Rsnd (Pair(x, y)) = {(y)};


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


xor_b : p -> {v | {(v)} = Rxor (p)}; 

map : (p -> {v | {(v)} = Rxor (p)}) -> l1 -> {vout | Rmem (vout) = Rmemxor (l1)};

xor : pl -> {vxor | Rmem(vxor) = Rmemxor (pl)};