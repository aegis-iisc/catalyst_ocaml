
	(* define a module for the Type_desc and define helper functions on them
         * It will be easier to move this module to types module*)
open Types
type t = Types.type_desc

let rec string_of_explist elist = 
    match elist with 
        | [] -> ""
        | e :: l -> (string_of_int e.id)^"::"^(string_of_explist l)


let rec toString t =
    match t with 
        | Tvar s -> (match s with Some s -> s | None -> "")
        | Tarrow (l1 ,e1, e2, l2) -> "[e1 -> e2]"
        | Ttuple listexp -> string_of_explist listexp 
        | _ -> "unKnown"
                             
let equal t1 t2 = 
  match (t1, t2) with 
        | (Tvar a1, Tvar a2) -> if (toString (Tvar a1)) = (toString (Tvar a2)) then true else false
        | (_,_) -> false

let sameType = equal

	
let rec keepAll f l =  
   match l with 
    [] -> []
    | x :: l' -> if (f x ) then (x :: keepAll f l') else (keepAll f l')
  
(* Implement types substitution *)

(* 'a * 'b -> 'b *)
let substTyvar ((tyd,tyvar),ty) = 
      let tyvarStrEq (v1,v2) = (equal v1 v2) 
         in
        match ty with
          Tvar tvar -> if (tyvarStrEq (Tvar tvar,tyvar)) 
            then tyd else ty
        | _ -> ty
      
    
let rec instantiateTyvars substs t = 
    match t with
    Tvar str -> Tvar str (* Redefine this after some thougth about how the substitution is carried out*)
    (*List.fold_right substTyvar substs t*)
  | Tarrow (l1,e1,e2,l2) ->Tarrow (l1,e1,e2,l2)(*l1, (instantiateTyvars substs e1.desc), instantiateTyvars e2.desc, l2)*)
  | _ -> t

let (compose) = fun f g x -> f (g x)
