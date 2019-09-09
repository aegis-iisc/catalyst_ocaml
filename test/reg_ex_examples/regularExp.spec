relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rlast (nil) = {()}
               | (cons (x, nil)) = {(x)}
               | (cons (x, xs)) = Rlast (xs);

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

relation Rfst (Pair (x,y)) = {(x)};

relation Rsnd (Pair (x,y)) = {(y)};

relation Rmem = Rhd*;

relation Rplmem (E) = {()} 
				| (L p) =  Rfst (p) U Rsnd (p) 
				| (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rplmem (pl));

relation Rpairs (E) = {()} 
				| (L p) =  (Rfst (p) X Rsnd (p)) 
				| (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rpairs (pl));

relation Rfla (E) = {()}
				| (L p) = (Rfst (p) X Rsnd (p))
				|  (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl)) U Rfla (pl) ;


relation Rnext_hd (nil) = {()}
                  | (cons (x, xs)) = {(Rhd(xs))};

relation Rnext_next_hd (nil) = {()}
                  | (cons (x, xs) = {(Rnext_hd(xs))};

regConcat : l1 -> l2 -> {l | Rplmem (l) = Rmem (l1) U Rmem (l2) /\
                             Robs(l1) U Robs(l2) C Rfla(l)}

regAlteration : l1 -> l2 -> {l | Rmem(l) = Rmem(l1) U Rmem(l2) /\ 
                                 Robs(l1) U Robs(l2) U Rmem(l1) X Rmem(l2) C= Robs(l)}

anchor_start : e -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                               Rhd(l) = e}

anchor_end : e -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                             Rlast(l) = e}

anchor_start_end : e -> e' -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                                         Rhd(l) = e /\
                                         Rlast(l) = e'}

anchor_exact : e -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                               {(e)} C= Rmem(l)}


reg_or : e -> e' -> e'' -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                                      Rhd(l) = e /\
                                      (Rnext_hd(l) = e' \/ Rnext_hd(l) = e'')}

reg_add : e -> e' -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                                Rhd(l) = e /\
                                Rnext_hd(l) = e'}

reg_zero_one : e -> e' -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                                     Rhd(l) = e /\
                                     ((Rnext_hd(l) = e' /\ not (Rnext_next_hd(l) = {(e')})) \/
                                      Rnext_hd(l) (not equal) e')}









