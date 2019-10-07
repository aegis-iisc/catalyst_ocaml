relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Robs = Rob*;
relation Roas = Roa*;

append : l1 -> l2 -> {l | Rmem(l) = Rmem(l1) U Rmem(l2) 
							/\ Robs(l) = Robs (l1) U Robs (l2) U (Rmem(l1) X Rmem(l2)) };								

