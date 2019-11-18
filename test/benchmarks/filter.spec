relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

relation Rsatisfy p (x) = if p x then {(true)} else {(false)};

relation Rbool p (cons(x, xs)) = Rsatisfy*;

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};

filter : f -> l -> {l' | Rmem(l') C= Rmem(l)}