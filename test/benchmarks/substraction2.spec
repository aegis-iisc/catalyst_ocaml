relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> (x1) = (y1)};
assume minus_one : n1 -> {vn | (vn) = (n1) -- (1)};

loop : n -> {l} -> {v | Rlen(v) = (n) /\ Rlen(l) = (n)};
