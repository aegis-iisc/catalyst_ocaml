relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;


relation Rmemjson 		(Str i) = {(i)}
						| (Num n) = {(n)}
						| (Obj f) = Rpmem (f);
			

relation Rpmem (Pair (j1, j2)) = Rmemjson(j1) U Rmemjson(j2);

						
relation Robsjson 		(Str i) = {()} X {()}
						| (Num n) = {()} X {()}
						| (Obj f) = Rpobs (f);


relation Rpobs (Pair (j1, j2)) = (Rmemjson(j1) X Rmemjson(j2)) U 
									Robsjson (j1) U Robsjson (j2);



relation RmemfstRes (ParseRes (l,r)) = Rmemjson (l);
relation RmemsndRes (ParseRes (l,r)) = Rmem (r);


relation RobsfstRes (ParseRes (l,r)) = Robsjson (l);
relation RobssndRes (ParseRes (l,r)) = Robs (r);




assume projl : p0 -> {pl | Rmemjson (pl) = RmemfstRes (p0)};
assume projr : p1 -> {pr | Rmem (pr) = RmemsndRes (p1)};


assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};

parse_json : input ->  {outpair | ( RmemfstRes(outpair) C Rmem (input) ) /\
								     RobsfstRes (outpair) C Robs (input)};

parse_field : ls -> {v| RmemfstRes(v) C Rmem (ls) /\
								     RobsfstRes (v) C Robs (ls)};


