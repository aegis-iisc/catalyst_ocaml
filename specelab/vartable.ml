open Types
  
exception VarNotFound

type 'b t = (Var.t,'b) Hashtbl.t

let empty () = 
	let empty_tbl = Hashtbl.create 2444 in 
	empty_tbl

  let mem m k = Hashtbl.mem m k

  let replace m k v = Hashtbl.add m k v
