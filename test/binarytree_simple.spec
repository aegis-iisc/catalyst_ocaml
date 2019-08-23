
relation Rroot (Node(l,n,r)) = {(n)} | (Leaf) = {()};
relation Rtmem = Rroot*;

insert : x -> t1 -> {v | Rtmem (v) = Rtmem(t1) U {(x)}};
