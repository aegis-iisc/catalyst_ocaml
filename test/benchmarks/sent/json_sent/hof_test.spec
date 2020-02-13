relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;


assume concat : l1 -> l2 -> { l |  Rmem(l) = Rmem(l1) U Rmem(l2) };



assume hof : ls -> {vhof | Rmem(vhof) C= Rmem (ls)};

assume higher_order : (l -> {v| Rmem(v) C= Rmem(l)}) -> 
				lst -> 
				{vt | Rmem(vt) C= Rmem(lst)};

test : lin -> {vout | Rmem(vout) C= Rmem (lin)};				
