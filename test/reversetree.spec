relation Rroot (Node(l,n,r)) = {(n)} | (Leaf n) = {(n)};
relation Rtmem = Rroot*;
relation Rto (Node (l,n,r)) = (Rtmem(l) X {(n)}) U ({(n)} X Rtmem(r))
                             U (Rtmem(l) X Rtmem(r))
 	           | (Leaf n) = {()};
relation Rtos = Rto*;


reversetree : t -> { tr | Rtmem(tr) = Rtmem(t)};


