relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 

assume eq : x1 -> y1 -> {veq | [veq=true] <=> (x1) = (y1)};

set_msb : l -> {v | Rlen(v) = Rlen(l) /\
                    Rhd(v) = {(1)}};