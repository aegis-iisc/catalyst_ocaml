relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;

assume raise : ex -> {vex | true};
assume is_non_empty : l -> {v | [v=true] <=> not (Rmem(l) = {()})};

head : ls -> { v1 | Rhd(ls) = {(v1)}};