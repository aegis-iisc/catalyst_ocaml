relation Rhd  (cons (x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*; 

cons : e -> l1 -> {l | Rmem(l) = {(e)} U Rmem(l1)};
