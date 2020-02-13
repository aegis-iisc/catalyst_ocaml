relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

assume eq : x1 -> y1 -> {veq | [veq=true] <=> (x1) = (y1)};

check_if_zero_or_not : l -> {v | [v = true] <=> {(0)} C Rmem(l)};






