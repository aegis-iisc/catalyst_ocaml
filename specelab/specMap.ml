module  type Key = sig
  type t 
  val equal : t*t -> bool
  val layout : t -> Layout.t
end 
module type Value =  sig
 type t
  val layout: t -> Layout.t
end 

module ApplicativeMap (Key : Key) (Value : Value) = 
struct
	module Key = Key
	module Val = Value 
	module L = Layout

	exception KeyNotFound of Key.t
  	
    type t = (Key.t * Val.t) list
  
  	let empty = []

  	let mem mp k =List.exists (fun (k', v) -> Key.equal (k, k')) mp

  	let find mp k =  
  		try 
        let found = List.find (fun (k', v) -> Key.equal (k, k')) mp in 
          snd (found)
      

  			(* let (a,b) = List.find (fun (k', v) -> Key.equal (k, k')) mp
          in
           b *)
  		with  
  		| Not_found -> raise (KeyNotFound k)

  	let add mp k v = (k,v) :: mp 

  	let remove mp k = List.remove_assoc k mp

  	let map f t = List.map f t
  	
    let toVector mp = Vector.fromList mp
  
    let layout mp = L.align (List.map (fun (k, v) ->
    L.seq [L.str "\n";Key.layout k; L.str " :: "; Val.layout v; L.str "\n"]) mp)



end 

