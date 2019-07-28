
open SpecMap
let strEq str1 str2 = (str1 = str2)  
module ConstantMapKey = 
       struct
         type t = string
         let equal (t1, t2)  =  strEq t1 t2
         let layout t = Layout.str t
       end

module ConstantMapValue = 
       struct
         type t = Z3_encode.ast
         let layout = Z3_encode.ast_layout
        end

module ConstMap = SpecMap.ApplicativeMap (ConstantMapKey) (ConstantMapValue)

type t = ConstMap.t
exception ConstNotFound of ConstantMapKey.t
  
let empty = ConstMap.empty
let mem = ConstMap.mem
let find env relId = 
    try (ConstMap.find env relId) 
  with 
  | (ConstMap.KeyNotFound k) -> raise (ConstNotFound k)

let add = fun env -> fun var desc -> ConstMap.add env var desc 

let remove = ConstMap.remove

let toVector = ConstMap.toVector

let layout = ConstMap.layout
