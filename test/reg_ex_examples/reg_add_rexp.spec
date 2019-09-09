relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

relation Rnext_hd nil = {()}
                  | (cons (x, xs)) = (Rhd(xs));

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)} };


and_fun : x2 -> y2 -> {andeq | [andeq=true] <=>  ([x2=true] /\ [y2=true])};

check_hd : el -> ls -> {bhd | [bhd=true] <=> Rhd(ls) = {(el)}};

get_next_element : e1 -> e1' -> l1 -> {b | [b=true] <=> ((Rhd(l1) = {(e1)}) /\ (Rnext_hd(l1) = {(e1')}))};

reg_add : e2 -> e2' -> l2 -> {l | (Rmem(l) = Rmem(l2)) /\
                                (Rhd(l) = {(e2)}) /\
                                (Rnext_hd(l) = {(e2')})};