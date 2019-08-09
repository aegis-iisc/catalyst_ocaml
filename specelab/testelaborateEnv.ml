(*@Var Env:
---------
[true : .  .  {v_2:bool |  v_2 = true }
false : .  .  {v_2:bool |  v_2 = false }
[] : .  .  {v_3:  'a list |  true }
:: : .  .   ->  {v_4:  'a list |  true }   v_9 : v_8 : {v_7:  'a list |  true } v_6 : {v_5:'a |  true }
concat : .  .   ->   ->  {l:  'a list |  (Rmem)(l) = ((Rmem)(l1) U (Rmem)(l2)) }   l2 : {v_1:  'a list |  true }   l1 : {v_0:  'a list |  true }
concat : .  .   ->   ->  {l:<?> |  (Rmem)(l) = ((Rmem)(l1) U (Rmem)(l2)) }   l2 : {v_1:<?> |  true }   l1 : {v_0:<?> |  true }
]
@Rel Env:
-----------

 Rhd  ::  {type = : @ss: :.   'a list :-> {TupleSort 'a}; map = {::::xxs => {(x)}[] => {()}}} 
   
 Rmem  ::  {type = : @ss: :.   'a list :-> {TupleSort 'a}; map = {::::xxs => ({(x)} U ({()} U (Rmem)(v_9.v_8)))[] => {()}}} 
   
 Rob  ::  {type = : @ss: :.   'a list :-> {TupleSort 'a'a}; map = {::::xxs => ({(x)} X (Rmem :'a)(xs))[] => {()}}} 
   
 Robs  ::  {type = : @ss: :.   'a list :-> {TupleSort 'a'a}; map = {::::xxs => (({(x)} X (Rmem :'a)(xs)) U ({()} U (Robs)(v_9.v_8)))[] => {()}}} 

@Param Rel Env:
--------------

 Rhd  ::  {typescheme = : @ss: :.   'a list :-> {TupleSort 'a}, def = \v_1. bind (Rhd : (v_1),\:v_0. {(v_0)})} 
   
 Rmem  ::  {typescheme = : @ss: :.   'a list :-> {TupleSort 'a}, def = \v_3. bind (Rmem : (v_3),\:v_2. {(v_2)})} 
   
 Rob  ::  {typescheme = : @ss: :.   'a list :-> {TupleSort 'a'a}, def = \v_6. bind (Rob : (v_6),\:v_4v_5. ({(v_4)} X {(v_5)}))} 
   
 Robs  ::  {typescheme = : @ss: :.   'a list :-> {TupleSort 'a'a}, def = \v_9. bind (Robs : (v_9),\:v_7v_8. ({(v_7)} X {(v_8)}))} 
*)

(*Elaborated VarEnv:
114 
115 :: :-> ('a_2)({1:'a_2 | true}, {2: 'a_2 list | true})
116               -> {v_6: 'a_2 list | true
										(*\phi_c*)	
117                                    /\ Robs(v_6) = (({(1)} X Rmem(2)) U (Robs(2) U {()}))
118                                    /\ Rob(v_6) = ({(1)} X Rmem(2))
119                                    /\ Rmem(v_6) = ({(1)} U (Rmem(2) U {()}))
120                                    /\ Rhd(v_6) = {(1)}}
121 concat :-> (){l1:<?> | true}
122               -> {l2:<?> | true}
123                   -> {l:<?> | Rmem(l) = (Rmem(l1) U Rmem(l2))}
124 nil :-> ('a_2){v_0: 'a_2 list | true
										(*\phi_n*)
125                                 /\ Robs(v_0) = {()}
126                                 /\ Rob(v_0) = {()}
127                                 /\ Rmem(v_0) = {()}
128                                 /\ Rhd(v_0) = {()}}
*)
open SpecLang
open SpecMap
module L = Layout
module VE = VarEnv 
module RE = RelEnv 
module PRE = ParamRelEnv
module TyD = TyD
module TyDB = TyDBinds
module RelId = RelId
module TS = TupSort
module SPS = SimpleProjSort
module PTS = ProjTypeScheme
module PSS = ProjSortScheme
module RefTy = RefinementType
module PRf = ParamRefType
module RefSS = RefinementSortScheme
module RefTyS = RefinementTypeScheme
module P = Predicate
module RP = P.RelPredicate
module BP = P.BasePredicate
module TypeSpec = RelSpec.TypeSpec
module PR = PrimitiveRelation
module SR = StructuralRelation


let elaborate () = 


(* [true : .  .  {v_2:bool |  v_2 = true }
false : .  .  {v_2:bool |  v_2 = false }
[] : .  .  {v_3:  'a list |  true }
:: : .  .   ->  {v_4:  'a list |  true }   v_9 : v_8 : {v_7:  'a list |  true } v_6 : {v_5:'a |  true }
concat : .  .   ->   ->  {l:  'a list |  (Rmem)(l) = ((Rmem)(l1) U (Rmem)(l2)) }   l2 : {v_1:  'a list |  true }   l1 : {v_0:  'a list |  true }
concat : .  .   ->   ->  {l:<?> |  (Rmem)(l) = ((Rmem)(l1) U (Rmem)(l2)) }   l2 : {v_1:<?> |  true }   l1 : {v_0:<?> |  true }
]
 *)
(*Var.t -> RefinementTYpeScheme*)
let ve = VE.empty in 
let true_RefTyS = RefTyS.T {tyvars = []; refss = RefSS.T {svars= []; prefty = PRf.T {params = []; refty = RefTy.fromTyD (TyD.Tbool)}} ; isAssume = false} in 
let true_tyBind = (Var.fromString "true", true_RefTyS ) in 

let false_RefTyS = RefTyS.T {tyvars = []; refss = RefSS.T {svars= []; prefty = PRf.T {params = []; refty = RefTy.fromTyD (TyD.Tbool)}} ; isAssume = false} in 
let false_tyBind = (Var.fromString "false", true_RefTyS ) in 



let ve = VE.add ve true_tyBind in 
let ve = VE.add ve false_tyBind in 

let tyDnil = (TyD.Tconstr(Tycon.fromString "list" , [TyD.Tvar (Tyvar.fromString "'a")])) in 
let nil_RefTyS = RefTyS.T {tyvars = []; refss = RefSS.T {svars= []; prefty = PRf.T {params = []; refty = RefTy.fromTyD tyDnil}} ; isAssume = false} in 
let nil_tyBind = (Var.fromString "[]", nil_RefTyS ) in 

let ve = VE.add ve nil_tyBind in 

let tyDCons2 = (TyD.Tconstr(Tycon.fromString "list" , [TyD.Tvar (Tyvar.fromString "'a")])) in 
let tyDCons1 = (TyD.Tvar (Tyvar.fromString "'a")) in
let tyDCons3 = (TyD.Tconstr(Tycon.fromString "list" , [TyD.Tvar (Tyvar.fromString "'a")])) in  

let prtrue = Predicate.truee() in 

(* 
:: :-> ('a_2)({v_8:'a_2 | true}, {v_6: 'a_2 list | true})
116               -> {v_5: 'a_2 list | true
										(*\phi_c*)	
117                                    /\ Robs(v_5) = (({v_8} X Rmem(v_6)) U (Robs(v_6) U {()}))
118                                    /\ Rob(v_5) = ({(v_8)} X Rmem(v_6))
119                                    /\ Rmem(v_5) = ({(v_8)} U (Rmem(v_6) U {()}))
120                                    /\ Rhd(v_5) = {(v_8)}}
121  *)


  let relIdRobs = RelId.fromString "Robs" in 
  let robsInst = RelLang.instOfRel relIdRobs in 
  
  let relIdRob = RelId.fromString "Rob" in 
  let robInst = RelLang.instOfRel relIdRob in 
  

  let relIdRhd = RelId.fromString "Rhd" in 
  let rhdInst = RelLang.instOfRel relIdRhd in 

  let relIdRmem = RelId.fromString "Rmem" in 
  let rmemInst = RelLang.instOfRel relIdRmem in 
  


  let instVar_v5 =  (Var.fromString "v_5") in 

  let instVar_v8 = (Var.fromString "v_7") in
  let instVar_v6 = (Var.fromString "v_6") in
  let instVar_l1= (Var.fromString "l1") in
  let instVar_l2 = (Var.fromString "l2") in
  let instVar_l = (Var.fromString "l") in
   
  
  (*Rmem(v_6)*)

  let r_expr_lhs = RelLang.R (robsInst, instVar_v5) in 
  let elhs1 = r_expr_lhs in 


  let rmem_v6  = RelLang.R (rmemInst, instVar_v6) in 

  let robs_v6 = RelLang.R (robsInst, instVar_v6) in 
  let tuple_v8 = RelLang.T [RelLang.Var (instVar_v8)] in 
 
(* cross v_8 , rmem_v6 *)	 	

let v_8_x_v_6 =  RelLang.X (tuple_v8, rmem_v6) in
let rhs_1 = RelLang.U ( (RelLang.U (v_8_x_v_6, robs_v6), RelLang.T [])) in 
let pred1 = RP.Eq (elhs1, rhs_1) in 

let pred2 = RP.Eq (RelLang.R(robInst, instVar_v5), v_8_x_v_6) in 

let pred3 = RP.Eq (RelLang.R (rmemInst, instVar_v5), RelLang.U (RelLang.U(tuple_v8, RelLang.R (rmemInst, instVar_v6)), RelLang.T []) ) in 
let pred4 = RP.Eq (RelLang. R (rhdInst, instVar_v5), tuple_v8) in 


let conj1 =  Predicate.Conj (Predicate.Rel pred1, Predicate.Rel pred2) in 
let conj2 = Predicate.Conj (conj1, Predicate.Rel pred3) in 
let conjCons = Predicate.Conj (conj2, Predicate.Rel pred4) in   





let ref_tydCons3 = RefTy.Base(RefTy.genVar(), tyDCons3, conjCons)  in 
let tyDConsSuffix =  TyD.Ttuple [tyDCons1; tyDCons2] in 
let ref_tyd = RefTy.fromTyD tyDConsSuffix in 
let ref_tyDCons = RefTy.Arrow ( (Var.fromString "", ref_tyd), ref_tydCons3) in  


let cons_RefTyS = RefTyS.T {tyvars = []; refss = RefSS.T {svars= []; prefty = PRf.T {params = []; refty = ref_tyDCons}} ; isAssume = false} in 
let cons_tyBind = (Var.fromString "::", cons_RefTyS ) in 

let ve = VE.add ve cons_tyBind in 


(* concat :-> (){l1:<?> | true}
122               -> {l2:<?> | true}
123                   -> {l:<?> | Rmem(l) = (Rmem(l1) U Rmem(l2))}
 *)


let concatVar = Var.fromString "concat" in 
let tyDl1 = (TyD.Tconstr(Tycon.fromString "list" , [TyD.Tvar (Tyvar.fromString "'a")])) in 
let refTyDl1 = RefTy.fromTyD tyDl1 in 

let tyDl2 = (TyD.Tconstr(Tycon.fromString "list" , [TyD.Tvar (Tyvar.fromString "'a")])) in 
let refTyDl2 = RefTy.fromTyD tyDl2 in 


let tyDl = (TyD.Tconstr(Tycon.fromString "list" , [TyD.Tvar (Tyvar.fromString "'a")])) in 
let refTyDl1 = RefTy.fromTyD tyDl1 in 
let rmem_l1  = RelLang.R (rmemInst, instVar_l1) in 
let rmem_l2  = RelLang.R (rmemInst, instVar_l2) in
let rmem_l  = RelLang.R (rmemInst, instVar_l) in

let pred3 = RP.Eq (rmem_l, (RelLang.U( rmem_l1 , rmem_l2 ))) in 


let ref_tydest  = RefTy.Base(Var.fromString "l", tyDl, Predicate.Rel pred3)  in 
let prefix1 = RefTy.Base ( (Var.fromString "l1"), tyDl1, Predicate.truee() ) in 
let prefix2 = RefTy.Base ( (Var.fromString "l2"), tyDl2, Predicate.truee() ) in 

let concatsuffix = RefTy.Arrow (( Var.fromString "", prefix2), ref_tydest)  in 
let concatrefTy = RefTy.Arrow (( Var.fromString "", prefix1), concatsuffix)  in 


let concat_RefTyS = RefTyS.T {tyvars = []; refss = RefSS.T {svars= []; prefty = PRf.T {params = []; refty = concatrefTy}} ; isAssume = false} in 

let concatTyD = (concatVar, concat_RefTyS) in 
let ve = VE.add ve concatTyD in 



 let  _ = Printf.printf "@Var Env Before:\n" in
  let  _ = Printf.printf "%s" ((VE.layout ve)) 
in 
ve 


