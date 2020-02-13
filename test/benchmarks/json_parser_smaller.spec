relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;



relation Rpmem (Pair (i, json)) = {(i)} U Rmemjson(json);
relation Rpobs (Pair (i, json)) = ({(i)} X Rmemjson(json)) U Robsjson (json);






relation Rmemjson (Empty) = {()}
						| (Str i) = {(i)}
						| (Num n) = {(n)}
						| (Obj p) = Rpmem (p);


relation Robsjson (Empty) = {()}
						| (Str i) = {()}
						| (Num n) = {()}
						| (Obj p) = Rpobs (p);


relation RmemfstRes (ParseRes (l,r)) = Rmemjson (l);
relation RmemsndRes (ParseRes (l, r)) = Rmem (r);


relation RobsfstRes (ParseRes (l,r)) = Robsjson (l);
relation RobssndRes (ParseRes (l,r)) = Robs (r);



assume projl : p0 -> {pl | Rmemjson (pl) = RmemfstRes (p0)};
assume projr : p1 -> {pr | Rmem (pr) = RmemsndRes (p1)};


assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};


assume concat : l1 -> l2 -> { l |  Rmem(l) = Rmem(l1) U Rmem(l2) /\ 
							Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};



parse_key : str -> { k | {(k)} = {(str)} };

parse_kvpair : input -> {outpair | RmemfstRes(outpair) C Rmem (input)/\
								RobsfstRes (outpair) C Robs (input)};

parse_value : inp -> {val | RmemfstRes (val) C Rmem(inp)/\
								RobsfstRes (val) C Robs (inp)};

parse_json : ls ->  {v | 	RmemfstRes (v) C Rmem(ls) /\
								RobsfstRes (v) C Robs (ls)};


						
