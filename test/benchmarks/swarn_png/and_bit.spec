relation Rfstpint (Pair_int (x,y)) = {(x)};

relation Rsndpint (Pair_int (x,y)) = {(y)};

and_bit : l -> {n | [n = 1] <=> ([Rfstpint(l) = {(1)}] /\ [Rsndpint(l)= {(1)}])};

