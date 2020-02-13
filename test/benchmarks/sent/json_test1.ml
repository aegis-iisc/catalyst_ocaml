
exception JSONEx of string 
type json = 
			| Num of int 
			| Str of int 
			| Obj of field 
and field = Pair of json * json 

type output  = ParseRes of (json) * (int list)

let raise ex = 
		let em = [] in 
		let z = 0 in 
		let defjson = Num z in 
		let defrem = em in  
		let resex = ParseRes (defjson,defrem) in 
		resex

let rec parse_field ls = 
	match ls with 
	[] -> raise (JSONEx "parsing empty field")
	| x :: xs -> 
		let res = parse_json xs in 
		res 
and parse_json inlist  = 
	match inlist with 
	[] -> 	raise (JSONEx "parsing empty")
	| x :: xs ->
		(*either parse a string, or parse a number *) 
			let  field = parse_field xs in 
			field 
		 	