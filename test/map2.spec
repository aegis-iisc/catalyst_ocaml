relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Robs = Rob*;
relation Roas = Roa*;
primitive relation RId = \z.{(z)};
map : (y -> {v2 | RId (v2) = {(c)} }) -> {l | Rhd (l) = {(c)}} 
  -> {v |  Rmem (v) C Rmem (l)};




