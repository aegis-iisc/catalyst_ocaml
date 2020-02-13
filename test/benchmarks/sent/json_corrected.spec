relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;


relation Rmemjson 		| (Str i) = {(i)}
						| (Num n) = {(n)}
						| (Obj f) = Rpmem (f);
									
relation Robsjson 		| (Str i) = {()}
						| (Num n) = {()}
						| (Obj f) = Rpobs (f);


relation Rpmem (Pair (j1, j2)) = Rmemjson(j1) U Rmemjson(j2);
relation Rpobs (Pair (j1, j2)) = (Rmemjson(j1) X Rmemjson(j2)) U 
									Robsjson (j1) U Robsjson (j2);



relation RmemfstRes (ParseRes (l,r)) = Rmemjson (l);
relation RmemsndRes (ParseRes (l, r)) = Rmem (r);


relation RobsfstRes (ParseRes (l,r)) = Robsjson (l);
relation RobssndRes (ParseRes (l,r)) = Robs (r);

(*constant relations*)
relation C_skip =  {(colon)} U {(comma)} U {(lbrace)} U {{rbrace}};

relation C_skip_plus = C_allowed_skipped_char U {(ws)};



relation R_ws_removed (l: list) (v: list) = ((Rem(l) - (Rmem(v)) = {(ws)}) \/ ((Rem(l) - (Rmem(v)) = {()}) (*the difference is only ws*)
							/\ 
							(Robs(l) - (Robs (v)) = ({(ws)} X Rmem (l)) U (Rmem (l) X {(ws)}) U ({(ws)} X {(ws)})) 
							);


relation R_ws_removed_for_output (l: list) (v:output) = 
							((Rem(l) - (RmemfstRes(v)) C= (C_skip_plus) \/ ((Rem(l) - (RmemfstRes(v)) = {()}) 
							/\ 
							(Robs(l) - (RobsfstRes (v)) C= (C_skip_plus X Rmem (l)) U (Rmem (l) X C_skip_plus}) U (C_skip_plus X C_skip_plus));



relation R_js_parsed input outpair = (Rmem (input) - RmemfstRes(outpair)) C= C_allowed_skippped_char_plus /\
								     RobsfstRes (outpair) C Robs (input) /\


(*function specs*)

parse_space : inp -> {ps | [ps=true] <=> Rhd(inp) = {(ws)}};




parse_spaces : ls -> {v | 	R_ws_removed ls v};



parse_token :  (f_ws : ( l -> {v_fws | R_ws_removed l v_fs})) ->
				ls ->
				(g :(inp -> {ret | R_js_parsed inp ret}) ) ->
				{v | R_ws_removed_for_output ls v /\
					R_js_parsed ls v};


parse_json : ls ->  {v | R_js_parsed ls v /\
							R_ws_removed_for_output ls v};

and parse_field : input -> {outpair | R_js_parsed input outpair /\
								R_ws_removed_for_output input outpair};




'R foo : (f : (arg : {arg : int | true} -> {ret  : bool | [ret = true] <=> ('R  arg) = Rid{("True")} /\
															 [ret = false] <=> ('R  arg) = Rid{("False")}) ->
   ((x : {x : int | true}) ->
    {v : int * bool | Rfst(v) = {(x)} /\ Rsnd(v) = 'R (x) })

relation R_f 