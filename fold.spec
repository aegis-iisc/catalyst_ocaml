relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Robs = Rob*;
relation Roas = Roa*;
relation qRm = Rhd*; 
relation qRo = Rob*;

fold_left : (ac -> y -> {v1 |  qRm(v1) = {(y)} U qRm(ac)
            /\ qRo(v1) = ({(x)} X qRm(ac)) U qRo(ac) }) -> accum  -> l -> {v | qRm(v) = Rmem(l) U qRm(accum)
             /\ qRo(v) = Roas(l) U qRo(accum) U (Rmem(l) X qRm(accum))};
