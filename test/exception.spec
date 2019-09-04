relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;


assume gt : x1 -> y1 -> {v1 | true}; 
assume raise : ex -> {vex | true};

compare : x -> y -> { v | {(v)} = {(x)} \/ {(v)} = {(y)}};