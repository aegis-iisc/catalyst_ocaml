open SpecLang
open SpecMap

let (<<) = fun f g x -> f (g x)


type def = 
  Prim of SpecLang.PrimitiveRelation.def
  | Bind of SpecLang.Bind.def
  

type reldesc = 
  {tys : SpecLang.ProjTypeScheme.t;
   def : def}

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
  
  let toString = fun {tys;def} ->
    let tyDS = ProjTypeScheme.toString tys in 
    let defStr = match def with 
          Bind bdef -> Bind.defToString bdef
        | Prim pdef -> PrimitiveRelation.defToString pdef
      in
        "{typescheme = "^tyDS^", def = "^defStr^"}"
    
  let layout t = (Layout.str << toString) t
end

module RelMap = ApplicativeMap (Key)(Value)

exception ParamRelNotFound of RelId.t

type t = RelMap.t
let empty = RelMap.empty
let mem = RelMap.mem
let find env relId = 
    try (RelMap.find env relId) 
  with 
  | (RelMap.KeyNotFound k) -> raise (ParamRelNotFound k)

let add = fun env -> fun (var,desc) -> RelMap.add env var desc 

let addUniterp = fun env -> fun (var,tys) -> RelMap.add env var {tys=tys; 
                                                             def=Bind Bind.BogusDef}

let remove = RelMap.remove

let toVector = RelMap.toVector

let layout = RelMap.layout


