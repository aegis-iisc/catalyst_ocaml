   let eq x1 y1 = true
   exception TestExp
   let raise ex = 
   		[]

   let check_hd e l = match l with 
    | [] -> false 
    | x :: xs -> let y = (eq x e) in 
                 if y then true else false

 (* Anchor ^ which matches any integer starting with the argument provided 
     ^The returns the string that starts with The *)
  let rec anchor_start e l1 = match l1 with 
      | [] -> raise TestExp
      | x :: xs -> 
      			let chr  = check_hd e l1 in 

      			if (chr) 
                   then l1
                   else raise TestExp


  (*One incorrect implementation *)
  (* let rec anchor_start e l1 = match l1 with 
      | [] -> raise TestExp
      | x :: xs -> 
            let chr  = check_hd e l1 in 

            if (chr) 
                   then [] (*returning an empty list*)
                   else raise TestExp                  *)