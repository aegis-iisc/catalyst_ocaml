open SpecLang
open SpecMap
module TyD = Types

let (<<) = fun f g x -> f (g x)

type reldesc = { ty : ProjTypeScheme.t;
                 map : (Con.t * Var.t list option * RelLang.expr) 
                     list}

let relIdStrEq = fun (v,v') -> 
  (RelId.toString v) = (RelId.toString v')

module Key = 
struct 
  type t = RelId.t
  let equal = relIdStrEq
  let layout = Layout.str << RelId.toString

end

module Value = 
struct
  type t = reldesc
  let toString = fun ({ty;map}) ->
    let tyDS = ProjTypeScheme.toString ty in 

    let conmap = "{" ^ (Vector.toString (
        fun (c,vlo,rexpr) ->
          let cstr = Con.toString c in 
          let vseq = match vlo with 
              None -> ""
            | Some vl -> Vector.toString (Var.toString) vl in 
          let trmstr = RelLang.exprToString rexpr
          in
          cstr ^ vseq ^ " => " ^ trmstr
      ) map) ^ "}"
    in
    "{type = "^tyDS^"; map = "^conmap^"}"

  let layout t = (Layout.str << toString) t
end

module RelMap = ApplicativeMap (Key)(Value)
exception RelNotFound of RelId.t
type t = RelMap.t
let empty = RelMap.empty
let mem = RelMap.mem
let find relId env= 
  try RelMap.find  relId env 
  with 
  | (RelMap.KeyNotFound k) -> raise (RelNotFound k)

let add = fun env -> fun (var,tys) -> RelMap.add env var tys 

let addUniterp = fun env -> fun (r,pts) -> RelMap.add env r {ty=pts; 
                                                             map=Vector.new0 ()}

let remove = RelMap.remove

let toVector = RelMap.toVector

let layout = RelMap.layout


