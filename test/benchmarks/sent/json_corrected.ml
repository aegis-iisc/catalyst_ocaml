type json = 
			| Num of int 
			| Str of int 
			| Obj of field 
and field = Pair of json * json 

type output  = ParseRes of (json) * (int list)

let parse_space inp = 
	if ((inp = 32) || (inp = 9) ) then 
		true  else 
		false


(*trims the white spaces using many1 ws/tab*)

let rec parse_spaces inp =
	match inp with 
	[] -> []
	| x :: xs -> 
		let one = parse_space x in 
		if(one) then 
			let trimmed = parse_spaces xs in 
			trimmed  
		else
			inp 	

let token p inp= 
	let ls = parse_spaces inp in 
	let res = p ls in 
	res


