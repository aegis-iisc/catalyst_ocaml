relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
last_element : l -> {v | Rmem(v) C Rmem(l) \/ Rmem(v) = {()}};

