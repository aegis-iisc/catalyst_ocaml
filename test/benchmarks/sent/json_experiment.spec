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
relation RmemsndRes (ParseRes (l, r)) = Rmem (r);


relation RobsfstRes (ParseRes (l,r)) = Robsjson (l);
relation RobssndRes (ParseRes (l,r)) = Robs (r);



assume projl : p0 -> {pl | Rmemjson (pl) = RmemfstRes (p0) /\ (Robsjson(pl) = RobsfstRes (p0))};
assume projr : p1 -> {pr | Rmem (pr) = RmemsndRes (p1) /\ (Robs(pr) = RobssndRes (p1))};



assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};


assume concat : l1 -> l2 -> { l |  Rmem(l) = Rmem(l1) U Rmem(l2) /\ 
							Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};



parse_num : ls -> {vnum | (RmemfstRes(vnum) C Rmem(ls)) /\
								RobsfstRes (vnum) C Robs (ls)/\
								RmemsndRes(vnum) C Rmem(ls)/\
								RobssndRes(vnum) C Robs(ls)};



parse_colon : ls -> {vcolon | (RmemfstRes(vcolon) C Rmem(ls)) /\
								RobsfstRes (vcolon) C Robs (ls) /\
								RmemsndRes(vcolon) C Rmem(ls) /\
								RobssndRes(vcolon) C Robs(ls)};



parse_field : fls -> {v| RmemfstRes(v) C Rmem (fls)  /\
								     RobsfstRes (v) C Robs (fls) /\
								     RmemsndRes(v) C Rmem(fls)/\
								     RobssndRes(v) C Robs(fls)};

parse_json : input ->  {outpair | ( RmemfstRes(outpair) C Rmem (input) ) /\
								     RobsfstRes (outpair) C Robs (input) /\
								     RmemsndRes(outpair) C Rmem (input)/\
								     RobssndRes(outpair) C Robs(input)};


