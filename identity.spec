relation Rhd  (cons (x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*; 

identity : l1 -> {l | Rmem(l) = Rmem(l1) };
