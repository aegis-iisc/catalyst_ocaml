relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 


relation Rfst (Pair(x, y)) = {(x)};
relation Rsnd (Pair(x, y)) = {(y)};


assume raise : ex -> {vex | true};


relation Rplmem (E) = {()}
                 | (L p) = (Rfst(p) U Rsnd(p))
                 | (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rplmem (pl));



relation Rplen  (E) = (0)
        | (L p) = (1)
        | (LCons (p, pl)) = (1) + Rplen(pl);



relation Rfstonly (E) = {()}
                  | (L p) = Rfst(p)
                  | (LCons (p, pl)) = Rfst(p) U Rfstonly(pl);


relation Rsndonly (E) = {()}
                  | (L p) = Rsnd(p)
                  | (LCons (p, pl)) = Rsnd(p) U Rsndonly(pl);




assume zip : l1 -> l2 -> {l |    (Rplen(l) = Rlen(l1)) /\ 
                          (Rplen(l) = Rlen(l2)) /\
                          (Rfstonly(l) = Rmem(l1)) /\
                          (Rsndonly(l) = Rmem(l2))};

