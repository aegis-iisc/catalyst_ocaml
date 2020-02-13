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


relation Rpobs (Pair (j1, j2)) = Robsjson (j1) U Robsjson (j2) U (Rmemjson (j1) X Rmemjson(j2));



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





parse_token :  (l -> {vfws | (Rmem(l) - Rmem(vfws) = {(32)}) \/ 
							(Rmem(l) - Rmem(vfws) = {()}) /\
							 (
							 	( (Robs(l) - Robs(vfws)) C= ({(32)} X {(32)}) 
							 								U ({(32)} X Rmem(l))) 
							 								\/ 
							 	((Robs(l) - Robs(vfws)) = {()})								
							 )	
							}) ->
				lst ->
				(inp -> {ret | (RmemfstRes(ret) C Rmem(inp)) /\
								(RmemsndRes(ret) C Rmem(inp)) /\
								 (RobsfstRes (ret) C Robs (inp))/\
									(RobssndRes(ret) C Robs(inp))								
								 }) ->

				{vt |       (RmemfstRes(vt) C Rmem(lst) /\
									RmemsndRes(vt) C Rmem(lst) /\
									  RobsfstRes (vt) C Robs (lst)/\
									RobssndRes(vt) C Robs(lst)								
								
							)
							 	
							
					};	 




parse_spaces : ls -> {vs| ((Rmem(ls) - Rmem(vs) = {(32)}) \/ 
							(Rmem(ls) - Rmem(vs) = {()})) /\
							 (
							 	( (Robs(ls) - Robs(vs)) C= ({(32)} X {(32)}) 
							 								U ({(32)} X Rmem(ls))) 
							 								\/ 
							 	((Robs(ls) - Robs(vs)) = {()})								
							 )	

							 };
							




parse_num : inp -> {vnum | (RmemfstRes(vnum) C Rmem(inp)) /\
								RobsfstRes (vnum) C Robs (inp)/\
								RmemsndRes(vnum) C Rmem(inp)/\
								RobssndRes(vnum) C Robs(inp)};

parse_num_ref : lnum -> {vlnum | RmemfstRes(vlnum) C Rmem(lnum) /\
								  RmemsndRes(vlnum) C Rmem(lnum) /\
								  RobsfstRes (vlnum) C Robs (lnum)/\
									RobssndRes(vlnum) C Robs(lnum)								
								};

parse_colon : inp -> {vcolon | (RmemfstRes(vcolon) C Rmem(inp)) /\
								RobsfstRes (vcolon) C Robs (inp) /\
								RmemsndRes(vcolon) C Rmem(inp) /\
								RobssndRes(vcolon) C Robs(inp)  
								};

parse_colon_ref : lss -> {vcolonr | (RmemfstRes(vcolonr) C Rmem(lss)) /\
								RobsfstRes (vcolonr) C Robs (lss) /\
								RmemsndRes(vcolonr) C Rmem(lss) /\
								RobssndRes(vcolonr) C Robs(lss)
								};




parse_field : inp -> {vf| RmemfstRes(vf) C Rmem (inp)  /\
								     RobsfstRes (vf) C Robs (inp) /\
								     RmemsndRes(vf) C Rmem(inp)/\
								     RobssndRes(vf) C Robs(inp)};

assume parse_field_ref : rfls -> {vr| RmemfstRes(vr) C Rmem (rfls)  /\
								     RobsfstRes (vr) C Robs (rfls) /\
								     RmemsndRes(vr) C Rmem(rfls)/\
								     RobssndRes(vr) C Robs(rfls)};


assume parse_json : inp ->  {outpair | ( RmemfstRes(outpair) C Rmem (inp) ) /\
								     RobsfstRes (outpair) C Robs (inp) /\
								     RmemsndRes(outpair) C Rmem (inp)/\
								     RobssndRes(outpair) C Robs(inp)};



assume parse_json_ref : rinlist -> {routpair | ( RmemfstRes(routpair) C Rmem (rinlist) ) /\
								     RobsfstRes (routpair) C Robs (rinlist) /\
								     RmemsndRes(routpair) C Rmem (rinlist)/\
								     RobssndRes(routpair) C Robs(rinlist)};
