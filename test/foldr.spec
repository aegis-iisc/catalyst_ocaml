relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Robs = Rob*;
relation Roas = Roa*;

fold_right : ( y -> ac -> {v1 |  Rmem(v1) = {(y)} U Rmem(ac)
            /\ Robs(v1) = ({(y)} X Rmem(ac)) U Robs(ac) }) -> l -> accum -> {v | Rmem(v) = Rmem(l) U Rmem(accum)
             /\ Robs(v) = Robs(l) U Robs(accum) U (Rmem(l) X Rmem(accum))};
