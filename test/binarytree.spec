relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;
relation Rroot (Node(l,n,r)) = {(n)} | (Leaf n) = {(n)};
relation Rtmem = Rroot*;
relation Rto (Node (l,n,r)) = (Rtmem(l) X {(n)}) U ({(n)} X Rtmem(r))
                             U (Rtmem(l) X Rtmem(r))
 	           | (Leaf n) = {()};
relation Rtos = Rto*;


concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\
               		Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};

preOrder : tr -> { lr | Rmem(lr) = Rtmem(tr) /\ Robs(lr) = Rtos(tr)};


