relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 
relation Rlentail (nil) = (0) | (cons(x, xs)) = Rlen(xs);


assume raise : ex -> {vex | true};

loop : l -> {v | Rhd(l) = {()} \/ Rhd(l) = Rlentail(l) };
