relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;
relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 

concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\ Rlen (l) = Rlen(l1) + Rlen(l2)};
