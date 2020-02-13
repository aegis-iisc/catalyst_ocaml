relation Rfstplist (Pair (x,y)) = {(x)};

relation Rsndplist (Pair (x,y)) = {(y)};

relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 

and_list : l -> {v | Rlen(v) = Rlen(l)};

