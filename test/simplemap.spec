relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Robs = Rob*;
relation Roas = Roa*;
primitive relation RId = \z.{(z)};

map : (y -> {v1 | RId(v1) = RId(y)}) -> l 
  -> {v | Robs (v) = Robs (l)};



