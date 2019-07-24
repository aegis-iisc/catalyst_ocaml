
exception VarNotFound
include Types


type 'b t = (Var.t,'b) Hashtbl
val empty : unit -> 'b t
val mem : 'b t -> Var.t -> bool
val replace : 'b t -> Var.t -> 'b -> unit
