(* module name : <signature> = <implemenation> *)
module type Key = sig

  type t 
  val equal : t*t -> bool
  val layout : t -> Layout.t
end 
module type Value =  sig

 type t
  (*val layout: t -> Layout.t*)

  
end 

module ApplicativeMap (K : Key) (V : Value) =
    
struct
	module Key = K
	module Val = V 
	module L = Layout

	exception KeyNotFound of Key.t
  type t = (Key.t * Val.t) list

  let empty = []

  	let mem map k  = List.exists (fun (k', v) -> Key.equal (k, k')) map
    (*returns the Value field of the key macthing the given key*)
  	let find map k  =  
  		try 
  			let found = List.find (fun (k', v) -> Key.equal (k, k')) map in 
        snd (found)
  		with  
  		| Not_found -> raise (KeyNotFound k)
  	let add map k v = (k,v) :: map 
  	let remove map k = List.remove_assoc k map

  	let map f t = List.map f t

  	
    let toVector map:t = map
   (*  let layout map = L.align (List.map (fun (k,v) ->
    L.seq [Key.layout k; L.str " :-> "; Val.layout v]) map)
 *)


end 

