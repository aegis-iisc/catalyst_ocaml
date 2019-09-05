relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};

check_hd : e -> l -> {b | [b=true] <=> Rhd(l) = {(e)}};

anchor_start : e1 -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                               (Rhd(l)) = {(e1)}};
