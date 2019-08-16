relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Robs = Rob*;
relation Roas = Roa*;
relation qRm = Rhd*; 
relation qRo = Rob*;

(*fold_left : (y -> ac -> {v1 |  qRm(v1) = {(y)} U qRm(ac)
 *           /\ qRo(v1) = ({(y)} X qRm(ac)) U qRo(ac) }) -> accum  -> l -> {v | qRm(v) = Rmem(l) U qRm(accum)
 *            /\ qRo(v) = Roas(l) U qRo(accum) U (Rmem(l) X qRm(accum))};
*)
fold_left : (ac -> y -> {v1 |  Rmem(v1) = {(y)} U Rmem(ac)
            /\ Robs(v1) = ({(y)} X Rmem(ac)) U Robs(ac) }) -> accum  -> l -> {v | Rmem(v) = Rmem(l) U Rmem(accum)
             /\ Robs(v) = Roas(l) U Robs(accum) U (Rmem(l) X Rmem(accum))};