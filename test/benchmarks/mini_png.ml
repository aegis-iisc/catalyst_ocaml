exception TestExp of string 

(*the return type for a parser (ParsedValue, Remaining List)*)
type listPair  = Pair of (int list) * (int list)


(* 
relation Rfst (Pair (l, r)) = {(l)}
relation Rsnd (Pair (l, r)) = {(r)}


relation Rlenfst (Pair (l, r)) = Rlen (l)
relation Rlensnd (Pair (l, r)) = Rlen (r)
 *)
let projl lp = 
	match lp with 
		| Pair (l, r) -> l
	
let projr lp = 
	match  lp with
	| Pair(l, r) -> r
	
(* 
let eq x1 y1 = true
   
let raise ex = [] *)

let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                        temp2 


(*The size of the input must be greater than the parameter n*)
(*parse_chunk : {n : int | n >= 0} -> {l1 | Rlen(l1) >= n} -> {v |
													Rlenfst (v)) = n /\ 
														Rlensnd (v) = (Rlen(l1) - n)}*)
let rec parse_chunk n inpc  = 
	match inpc with 
		[] -> Pair ([], [])
		| x :: xs ->  let () = Printf.printf "%s" (" iter n "^(string_of_int n)^"\n") in 
					  if (n = 0) then 
					  	Pair ([], inpc)
					  else 
					    let acc_chunk = parse_chunk (n-1) xs in
					    let t1 = projl acc_chunk in 
					    let rem = projr acc_chunk in  
					  	let res = x :: t1 in
					  	Pair (res, rem)
					 
(*parse_crc : l ->  {v : Pair(l1, l2) | Rlen (l) > 0 /\ 
										Rlen (Rfst (v)) = 1 /\ 
										Rlen (Rsnd (v)) = Rlen(l) - 1)}*)
let parse_crc input  = 
	match input with 
		[] -> raise (TestExp "CRC ill-formed")		  
		| x :: [] -> let crc_res = [x] in 
					  Pair (crc_res, [])
		| _ ->
			raise (TestExp "CRC ill-formed")			  


(*parse_png : l1 -> {v | Rlen (v) = Rlen (l1) /\
							Rmem (v) = Rmem (l1) /\
							Robs (v) = Robs (l1) }*)
let parse_png inp = 
	match inp with 
		[] -> []
		| x :: xs -> let len = x in 
					 let () = Printf.printf "%s" (string_of_int len) in 
					 (*parse the chunk*)
					 let parsed_chunk_res = parse_chunk len xs in
					 let chunk = projl parsed_chunk_res in 
					 let rem = projr parsed_chunk_res in 
					 (*parse the crc on the remainder*)
					 
					 let ()= Printf.printf "%s" "CRC " in 
					 let () = List.iter (fun i -> Printf.printf "%s" (string_of_int i)) rem in 	  

					 let parsed_crc_res = parse_crc rem in
					 let crc = projl parsed_crc_res in 
					 let rem1 = projr parsed_crc_res in
					 (*combine the results*)  
					 let parsed_body = concat chunk crc in 
					 let res = x :: parsed_body in 
					 res



(* 
let () = 
		let src = [-8;51;1;2;3;4;5;6;7;8] in 
        let parsedList  = parse_png src  in 
        let () = List.iter (fun i -> Printf.printf "%s" (string_of_int i)) parsedList in 
        () 

  *)
  let rec string_of_intlist ls = 
    match ls with 
      []-> "[]"
      | x :: xs -> "[ "^(string_of_int x)^" : "^(string_of_intlist xs)^" ]" 

(*@param src : list header, followed by a data, such that header gives the length of the remaining data*)
      
let ()  = 
	  let src = [-8;51;1;2;3;4;5;6;7;8] in 
      
      let parsedList  = parse_png src  in 
      let () = Printf.printf "%s" ("\n test1 ") in 
      let () = Printf.printf "%s" ("\ninput list "^(string_of_intlist src)) in 
      let () = Printf.printf "%s" ("\nparsed list "^(string_of_intlist parsedList)) in 
     
        ()  

