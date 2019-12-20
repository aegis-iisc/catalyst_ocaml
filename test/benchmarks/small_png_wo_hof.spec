relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;
relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 
relation Rlentail (nil) = (0) | (cons(x, xs)) = Rlen(xs);
relation Rhdn (nil) = (0) | (cons(x,xs)) = (x);

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> (x1) = (y1)};
assume minus_one : n1 -> {vn | (vn) = (n1) -- (1)};

loop : n -> {l1} -> {v1 | Rlen(v1) = (n) /\ Rlen(l1) = (n) };

parse_png : {inp}  ->  {v | Rhd(inp) = {()} \/ Rhdn (inp) = Rlentail (v)};




