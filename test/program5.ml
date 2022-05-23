

(* {char -> {v : bool | v = true <=> ch \in [0...9]} } *)
let is_digit digit =
    match digit with
     '0' .. '9' -> true
    | _ -> false

(*ch : {v : char| v \in [0...9]} -> {v : int | v = Intof (ch)}*)
let int_of ch = 
		(int_of_string (Char.escaped ch))
	


(*parseTag : {ls : list} -> {v | |v| = |ls|-2 \/ |v| = []}*)
let parseTag ls = 
	match ls with 
	[] -> []
	| x :: xs -> 
		if (x = 'T') then 	
			match xs with 
				[] -> []
				| y :: ys -> 
					if (y = 'T') then 
						ys
					else 
					[]	 	
		else 
			[]		


let program5 ls =
	
	match ls with 
	[] -> ([], ls, false) (*error = false |value| = [] *)
	| x :: xs -> 
		(*context := 
			|ls| = 1 + |xs|
			hd(ls) = x*)

		let (fuel, err) = 
			if (is_digit x) then
				(*true clause , x \in [0...9]*) 
				(int_of x, false)
				(*error = false /\ v = IntOf (x)*) 
	     	else
	     	 	(-1, true)
	     	 	(*error = true /\ v = -1*)	
		in  

		(*error = true \/ false /\ fuel = IntOf(x) \/ -1
			how can we be path sensitive here
		*)
		if (err = false) then 

			(*error = false /\ fuel = IntOf (x)(if clause)*)
			(*skip two 'T' char*)
			let (rem, errTag) = 
				if ((List.length xs) >= 2)
					then (parseTag xs, false) 

				else 
					([], true) 
			
			(* 	match xs with 
					[] -> error := true; [] (*error = true /\ rem = []*)
					| x1 :: xs1 -> 
						(*context :: |xs| =  |xs1| +1 *)
						if (List.length |xs1| )
						if (x1 = 'T') then xs1 (*error = false /\ rem = xs1*)
						else 
							((error := true);[]) (*error = true /\ rem = []*)
 *)
			in 			
			(*error = false /\ rem = xs1 /\ fuel = IntOf(x) \/ error = true /\ rem = [] /\ fuel = -1*)	
			if (errTag = false ) then
				(*if clause :: error = false /\ rem = xs1*)
				
				(*apporoach 1
					1.prefilter
					2. keep remaining list *)
				(* let fold_arg (n, acc_res, remain) el = 
					if (!error = true)
						(n, [], remain)
					else 
						if(n=0) then
							(n, acc_res, remain)
						else
							if(el = '+') then 
								let 
								(n-1, el::acc_res)
							else 
								error :=true;
								(n, [])
  

				in 
				let (n, res) = List.fold_left fold_arg rem in 

				 *)
 				(*Inv :: fuel + |acc_res| = ell**)
 			(* 	Can we prove the type by the invariant
 			 *)	(*loop : n : int -> acc_res : list -> ell : list -> {(v1, v2) | error = false /\ |v1| = |acc_res| + n /\ |v2| = ell - n
 																				\/ error = true /\ |v1| = []}*)
				let rec loop n (acc_res) ell =
					if(n= 0) then 
						(acc_res, ell ,false)
					else 
						match ell with 
							[] -> 
								([], ell, true)
							| y :: ys -> 
								if(y = '+') then 
									loop (n-1) (y::acc_res) (ys)
								else 
									([], ell, true)

				in 
				
				loop fuel [] rem 
				(* error = false => fuel = IntOf (x) /\ |v1| = fuel /\  |v2| = |ell| - fuel
					\/ error = true => v1 = [] *)
			else
				(*else clause error = true /\ |rem| = []  *)
				([], rem, true)
				(*error = true /\ fuel = IntOf (x) /\ v1 = []*)
		
					
		else 
			(*else clause  error = true /\ fuel = -1*)
			([], ls, true)		
			(*error = true /\ fuel = -1 /\ v1 = []*)
		

			(*to further prove the property we need |xs| = |ls| -1 *)
