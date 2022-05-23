relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;


(*membership and ordering relations over the output type of the parser, which is a pair Pair <json, int list>*)
relation Rpmem (Pair (i, json)) = {(i)} U Rmemjson(json);
relation Rpobs (Pair (i, json)) = ({(i)} X Rmemjson(json)) U Robsjson (json);

(*membership relation of a json object*)
relation Rmemjson (Empty) = {()}
						| (Str i) = {(i)}
						| (Num n) = {(n)}
						| (Obj p) = Rpmem (p);

(*ordering relation of a json object*)
relation Robsjson (Empty) = {()}
						| (Str i) = {()}
						| (Num n) = {()}
						| (Obj p) = Rpobs (p);


(*pojections for membership and ordering relation over the output type*)

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


(*checks if the input char is a ws, i.e. tab or space, I have used 'ws' as a symbol for whitespace characters *)

parse_space : inp -> {ps | [ps=true] <=> Rhd(inp) = {(ws)}}; 


(*the relations try to capture the notion, that the parsing of whitespaces, gives a new input list with ws character removed from the head of the list
1. This may lead to removal of ws from the membership (if ws is only in the beginning , for example [_ _ _ 1 3 4]) or not , for example [_ _ _ 1 _ 3_ 4], using _ for ws
2 .Similarly, the ordering relations are also defined*)


parse_spaces : ls -> {v |  
							((Rem(ls) - (Rmem(v)) = {(ws)}) \/ ((Rem(ls) - (Rmem(v)) = {()}) (*the difference is only ws*)
							/\ 
							(Robs(ls) - (Robs (v)) = ({(ws)} X Rmem (ls)) U (Rmem (ls) X {(ws)}) U ({(ws)} X {(ws)})) 
							/\ 
							(Rlen(v) <= Rlen (ls)) ) }


(*a token function, which takes a parser and an input list, removes ws from  the input and applies the parser on the trimmed list*)
token : f: (ls -> vp) -> {inp | (*relation input is ls with some possible whitespaces*) \phi } ->  
																						{v |   (*the output of the token is same as the output of the input parser*)
																						RmemfstRes(v) = RmemfstRes(vp) /\ 
																						RobsfstRes(v) = RobsfsResf(vp) /\
																						RmemsndRes(v) = RmemsndRes(vp) /\ 
																						RobssndRes(v) = RobssndRes(vp) };

where \phi = 
Rmem(inp) - Rmem (ls) = {(ws)} \/ Rmem(inp) - Rmem (ls) = {()}
/\ 
Robs(inp) - (Robs (ls)) = ({(ws)} X Rmem (ls)) U () (Rmem (ls) X {(ws)}) U ({(ws)} X {(ws)})) 
/\ 
(Rlen(inp) >= Rlen (ls))};



parse_key : str -> { k | {(k)} = {(str)} };

parse_kvpair : input -> {outpair | RmemfstRes(outpair) C Rmem (input)/\
								RobsfstRes (outpair) C Robs (input)};

parse_value : inp -> {val | RmemfstRes (val) C Rmem(inp)/\
								RobsfstRes (val) C Robs (inp)};

parse_json : ls ->  {v | 	RmemfstRes (v) C Rmem(ls) /\
								RobsfstRes (v) C Robs (ls)};

		
