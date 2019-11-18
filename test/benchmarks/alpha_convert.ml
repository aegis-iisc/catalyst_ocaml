exception TestExp 
type exp = 
        | Var of string
        | App of (exp * exp)
        | Abs of (string* exp)

let eq x1 y1 = true
   
let raise ex = []

(* We need to define exception for the type exp *)

let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                        temp2 

let rec remove_element x l = match l with 
  | [] -> []
  | y :: ys -> let z = eq x y in
               let result = remove_element x ys in 
               if z then result 
                    else y :: result

let rec exists e l1 = 
	match l1 with 
	  | [] -> false 
	  | x :: xs -> let y = eq e x in 
	               if y then true 
	                    else let b = exists e xs in b 

 let rec freeVars e = match e with 
   | Var x -> [x]
   | App (e1, e2) -> let x = freeVars e1 in 
                     let y = freeVars e2 in   
                     let z = concat x y in 
                     z 
   | Abs (x, e) -> let fv = freeVars e in 
                   let result = 
                   remove_element x fv in 
                   result 

let rec createNewName fvs id = 
	   let value = id ^ "'" in 
	   let t = exists value fvs in 
	   if t then let result = createNewName fvs value in result 
	        else value


let rec subst (e1:exp) (id : string) (e2 : exp) = match e1 with 
   | Var id' -> let z = eq id id' in 
                if z then e2 else e1 
   | App (e3, e4) -> let e3' = subst e3 id e2 in 
                     let e4' = subst e4 id e2 in 
                     App (e3', e4')
   | Abs (id', e5) -> let r = eq id id' in 
                      if r then e2 
                           else let fv_e1 = freeVars e1 in 
                                let t = exists id' fv_e1 in 
                                if t then subst (alphaConvert e1) id e2  
                                     else let result = subst e5 id e2 in 
                                          Abs (id', result)
and alphaConvert e = match e with 
   | Abs (id, e') -> let fv_e' = freeVars e' in 
                     let id' = createNewName fv_e' id in 
                     Abs(id', subst e' id (Var id')) 
   | _ -> raise "TestExp"
