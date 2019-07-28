
open SpecLang
open SpecMap
let strEq str1 str2 = (str1 = str2)  

module RelMapKey = 
       struct
         type t = string
         let equal (t1, t2)  =  strEq t1 t2
         let layout t = Layout.str t 
       end


module RelMapValue = 
       struct
         type t = Z3_encode.struc_rel
         let layout =Z3_encode.sr_layout
       end

module RelMap = SpecMap.ApplicativeMap (RelMapKey) (RelMapValue)


type t = RelMap.t
exception  RelNotFound of string
  
let empty = RelMap.empty
let mem = RelMap.mem
let find env relId = 
    try (RelMap.find env relId) 
  with 
  | (RelMap.KeyNotFound k) -> raise (RelNotFound ("Rel "^k^" not found"))

let add = fun env -> fun var desc -> RelMap.add env var desc 

let remove = RelMap.remove

let toVector = RelMap.toVector

let layout = RelMap.layout
