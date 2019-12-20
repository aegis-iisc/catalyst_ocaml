relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 
relation Rlentail (nil) = (0) | (cons(x, xs)) = Rlen(xs);
relation Rhdn (nil) = (0) | (cons(x,xs)) = (x);
relation Rsum (nil) = (0) | (cons(x,xs)) = ((x) + Rsum(xs));

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> (x1) = (y1)};
assume minus_one : n1 -> {vn | (vn) = (n1) -- (1)};
assume add : i1 -> i2 -> {vs | (vs) = (i1) + (i2)};


foldl : (val -> ac -> {vacc | (vacc) = (val) + (ac)}) -> acc -> l -> {vf | (vf) = (acc) + Rsum(l) }; 
fold_sum : ls -> {v | Rsum(ls) = (v)};



