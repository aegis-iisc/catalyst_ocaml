relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;
relation Rlen (nil) = (0) | (cons(x,xs)) = (1) + Rlen(xs);


relation Rmemjson 		(Str i) = {(i)}
						| (Num n) = {(n)}
						| (Obj f) = Rpmem (f);
			

relation Rpmem (Pair (j1, j2)) = Rmemjson(j1) U Rmemjson(j2);

						
relation Robsjson 		(Str i) = {()} X {()}
						| (Num n) = {()} X {()}
						| (Obj f) = Rpobs (f);


relation Rpobs (Pair (j1, j2)) = Robsjson (j1) U Robsjson (j2);



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



parse_spaces : ls -> {vs| ((Rmem(ls) - Rmem(vs) = {(32)}) \/ 
							(Rmem(ls) - Rmem(vs) = {()})) /\
							 (
							 	( (Robs(ls) - Robs(vs)) = ({(32)} X {(32)}) 
							 								U ({(32)} X Rmem(ls))) 
							 								\/ 
							 	((Robs(ls) - Robs(vs)) = {()})								
							 )	

							 };
							





parse_token :  (l -> {vfws | Rmem(vfws) U {(32)} C= Rmem(l) /\
										(
							 	( (Robs(l) - Robs(vfws)) = ({(32)} X {(32)}) 
							 								U ({(32)} X Rmem(l))) 
							 								\/ 
							 	((Robs(l) - Robs(vfws)) = {()})								
							 )}) ->
				lst ->
				(inp -> {ret | (RmemfstRes(ret) C Rmem(inp)) /\
								(RmemsndRes(ret) C Rmem(inp)) }) ->
				{vt |       (RmemfstRes(vt) C Rmem(lst) /\
									RmemsndRes(vt) C Rmem(lst)

							)
							 	
							
					};	 




assume parse_num : ls -> {vnum | (RmemfstRes(vnum) C Rmem(ls)) /\
								RobsfstRes (vnum) C Robs (ls)/\
								RmemsndRes(vnum) C Rmem(ls)/\
								RobssndRes(vnum) C Robs(ls)};



assume parse_colon : ls -> {vcolon | (RmemfstRes(vcolon) C Rmem(ls)) /\
								RobsfstRes (vcolon) C Robs (ls) /\
								RmemsndRes(vcolon) C Rmem(ls) /\
								RobssndRes(vcolon) C Robs(ls)};



assume parse_field : fls -> {v| RmemfstRes(v) C Rmem (fls)  /\
								     RobsfstRes (v) C Robs (fls) /\
								     RmemsndRes(v) C Rmem(fls)/\
								     RobssndRes(v) C Robs(fls)};

assume parse_json : input ->  {outpair | ( RmemfstRes(outpair) C Rmem (input) ) /\
								     RobsfstRes (outpair) C Robs (input) /\
								     RmemsndRes(outpair) C Rmem (input)/\
								     RobssndRes(outpair) C Robs(input)};


