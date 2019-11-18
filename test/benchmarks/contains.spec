relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};

exists : e -> l -> {v | v = true | {(e)} C Rmem(l) /\ not (Rmem(l)) = {()}}

contains : l1 -> l2 -> {v1 | v1 = true | Rmem(l1) C= Rmem(l2) /\ not (Rmem(l2) = {()})} 

