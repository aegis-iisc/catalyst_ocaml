relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Robs = Rob*;
relation Roas = Roa*;
primitive relation RId = \z.{(z)};

relation Rfst(Pair(x,y)) = {(x)};
relation Rsnd(Pair(x,y)) = {(y)};

zip : l1 -> l2 -> {v | ((Rmem Rfst) v = (Rmem RId) l1)
			};




