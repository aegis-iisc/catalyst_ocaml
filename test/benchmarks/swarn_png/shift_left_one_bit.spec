relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;


relation Rsnd nil = {()}
              | (cons(x, xs)) = Rhd (xs);


concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\ 
										Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};

shift_left_one_bit : l3 -> {v | Rmem(v) = (Rmem(l3) - Rhd(l3)) U {(0)}  /\
                                Robs(v) = ((Robs(l3) - (Rhd(l3) X Rmem (l3))) U (Rmem(v) X {(0)})) /\
                                Rhd(v) = Rsnd(l3)};