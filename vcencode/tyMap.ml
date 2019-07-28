
open SpecLang
open SpecMap
module TypeDMapKey =
       struct
         type t = TyD.t
         let equal(t1,t2)  =  TyD.sametype t1 t2
         let layout = Layout.str << TyD.toString 
       end

module TypeDMapValue =
       struct
         type t = Z3_encode.sort
         let layout = Z3_encode.sort_layout
               end


module TyMap = SpecMap.ApplicativeMap (TypeDMapKey) (TypeDMapValue) 

exception TyDNotFound of TypeDMapKey.t
  
type t = TyMap.t

let empty = TyMap.empty
let mem = TyMap.mem
let find t relId = 
    try (TyMap.find t relId) 
  with 
  | (TyMap.KeyNotFound k) -> raise (TyDNotFound k)

let add = fun t -> fun tyd sort -> TyMap.add t tyd sort 

let remove = TyMap.remove

let toVector = TyMap.toVector

let layout = TyMap.layout
