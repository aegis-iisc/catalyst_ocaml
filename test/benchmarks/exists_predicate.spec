relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};
or_fun : x3 -> y3 -> {veqor | [veqor=true] <=> ([x3=true] \/ [y3=true])};

exists_predicate : f -> l -> {v = true | }