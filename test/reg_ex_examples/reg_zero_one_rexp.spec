relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)} };

relation Rnext_hd nil = {()}
                  | (cons (x, xs)) = (Rhd(xs));

relation Rnext_next_hd (nil) = {()}
                  | (cons (x, xs)) = (Rnext_hd(xs));

and_fun : x1 -> y1 -> {veq | [veq=true] <=> (({(x1)}={(true)}) /\ ({(y1)}={(true)}))};

or_fun : x1 -> y1 -> {veq | [veq=false] <=> (({(x1)}={(false)}) /\ ({(y1)}={(false)}))};

not_fun : x -> {veq | [veq=true] <=> ({(x1)}={(false)})};

get_tail : l -> {l' | Rmem(l') C= Rmem(l) /\ Robs(l') C= Robs(l)};

check_hd : e -> l -> {b | [b=true] <=> Rhd(l) = {(e)}};

get_next_element : e -> e' -> l -> {b | [b=true] <=> ((Rhd(l) = {(e)}) /\ (Rnext_hd(l) = {(e')}))};

get_next_next_element : e -> e' -> l -> {b | [b=true] <=> ((Rhd(l) = {(e)}) /\ (Rnext_next_hd(l) = {(e')}))};


reg_zero_one : e -> e' -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                                     Rhd(l) = {(e)} /\
                                     ((Rnext_hd(l) = {(e')} /\ not (Rnext_next_hd(l) = {(e')})) \/
                                      not (Rnext_hd(l) = {(e')}))};