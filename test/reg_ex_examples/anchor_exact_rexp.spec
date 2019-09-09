relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};

mem_check : e -> l -> {b | [b=true] <=> {(e)} C= Rmem(l)};

anchor_exact : e1 -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                               {(e1)} C= Rmem(l)};