relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

assume eq : x1 -> y1 -> {veq | [veq=true] <=> (x1) = (y1)};

add_zeros : n -> acc -> {l | {(0)} C Rmem(l)};






