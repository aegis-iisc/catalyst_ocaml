(*
 * Variable Environment Creation*)
open SpecLang
module L = Layout
open SpecMap

let (<<) = fun f g x -> f (g x)

type tyscheme = RefinementTypeScheme.t
let varStrEq = fun (v,v') -> (Var.toString v) = (Var.toString v')

module Key = 
struct
  type t = Var.t
  let equal = varStrEq 
  let layout = L.str << Var.toString
end

module Value = 
struct
  type t = RefinementTypeScheme.t
  let  layout = RefinementTypeScheme.layout
end


module VarMap = ApplicativeMap (Key) (Value)

exception VarNotFound of Var.t

type t = VarMap.t

let empty = VarMap.empty

let mem = VarMap.mem

let find var env = 
  try VarMap.find  var  env with
  | (VarMap.KeyNotFound k) -> raise (VarNotFound k)

let add = fun env -> fun (var,tys) -> VarMap.add env var tys 

let remove = VarMap.remove

let toVector = VarMap.toVector

let string_var_type_pair (v, ty) = (Var.toString v)^" : "^(Layout.toString (RefinementTypeScheme.layout ty)) 
let layout mp = (List.fold_left (fun acc (v, ty) -> acc^string_var_type_pair(v,ty)^"\n") "[" mp)^"]" 


