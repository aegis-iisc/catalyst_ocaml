(* This module , generates the environment Gamma from the SpecLang generated from 
   the relational specifications and the ANormal form for the ocaml program*)
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


module SPSBValue = struct
  type t = {dom : TyD.t option ref; range : SVar.t}
  let layout = fun _ -> L.empty
end
module SPSBKey =  struct 
  type t = RelId.t
  let equal (s1, s2)  = RelId.equal (s1, s2)
  let layout = fun t -> L.empty  
end
module SPSBinds = ApplicativeMap (SPSBKey) (SPSBValue)



exception ElebEnvFail of string  

module SPSB = SPSBinds

module PrimRE = ApplicativeMap (
  struct
    type t = RelId.t
    let equal (s1, s2)  = RelId.equal(s1, s2)
    let layout = fun t -> L.empty  
  end)
    (struct
      type t = PrimitiveRelation.def
      let layout = fun _ -> Layout.empty
    end)

(* let Assert = Control.assert *)
let ($) = fun f arg -> f arg

let (<<) f g x = f (g x)

let tyconEq = Types.TypeOps.equal 

(* -- Function duplicated from SpecVerify -- *)
let count = ref 0
let  getUniqueId symbase =
  let id = symbase ^ (string_of_int(!count)) in 
  let _ = count := !count + 1 in 
  Var.fromString id 

let genVar () =  getUniqueId "x_" 
let newLongVar = fun (var,fld) -> Var.fromString $ 
                                  (Var.toString var)^"."^(Var.toString fld)
let empty = fun _ -> Vector.new0 ()
let emptycs = fun _ -> []
let mergecs = List.concat
let solvecs = TS.solvecs
(* 
 let get_base_td (tdc : (TyDB.TydMap.Key.t * (TyDB.TydMap.Key.t * Types.type_desc))) 
        : Types.type_desc =          
          let (a,(b,c)) = tdc in 
              c  

let get_full_from_td (tdc :  TyDB.TydMap.Key.t *Types.type_desc)
      =
          let a = fst tdc in 
         (a, (a ,tdc))

let get_full_from_td_list (ls ) = 

         List.map get_full_from_td ls


 let get_base_td_list (ls : (TyDB.TydMap.Key.t * (TyDB.TydMap.Key.t * Types.type_desc)) list) 
      : Types.type_desc list =
         List.map get_base_td ls  

 *)
let assertEmptyCs = fun cs -> match cs with
    [] -> ()
  | _ -> raise (ElebEnvFail "sort inference impossible")


let bootStrapBools ve = 
  let boolTyD = TyD.makeTbool () in 
  let tvid = Var.fromString (Con.toString Con.truee) in 
  let  fvid = Var.fromString ( Con.toString Con.falsee) in 
  let RefTy.Base (v,t,_) = RefTy.fromTyD boolTyD in 
  let eqPred1 = P.baseP ( BP.varBoolEq (v,true) ) in 
  let eqPred2 = P.baseP (  BP.varBoolEq (v,false) ) in 
  let empty = Vector.new0 () in
  let tTyS = RefTyS.generalizeRefTy (empty, 
                                     RefTy.Base (v,t,eqPred1)) in 
  let fTyS = RefTyS.generalizeRefTy (empty, 
                                     RefTy.Base (v,t,eqPred2)) in
  let ve' = VE.add ve (tvid,tTyS) in 
  let ve'' = VE.add ve' (fvid,fTyS)
  in
  ve''

(* let  elabDatBind  ve (cons,tyvars,tycon) =
   let destTyD = TyD.makeTconstr (tycon, tyvars) in 
   let elabCon = fun ve (arg,con) ->
    let vid = Var.fromString (Con.toString con) in 
    let conTyD = match arg with 
      | None -> destTyD
      | Some argTy -> TyD.makeTarrow argTy destTyD in 
    let conTyS = RefTyS.generalizeRefTy (tyvars, RefTy.fromTyD conTyD)
    in
    VE.add ve (vid,conTyS)

   in
   List.fold_left elabCon ve cons 
*)
let toRefTyS refTy = RefTyS.generalize (Vector.new0(), 
                                        RefSS.fromRefTy refTy)





open Typedtree 



let rec elabExpr ve expr = 
  let () = Printf.printf "%s" "elabExpr " in 
  let exp_desc = expr.exp_desc in 
  match exp_desc with 
  | Texp_let (rec_flagm, value_bindings, expression) ->
      List.fold_left elabValueBind ve value_bindings 

  | Texp_function (_,ls_case,_) ->
      List.fold_left elabCase ve ls_case

  | Texp_match (e1,ls_case,_,_) ->
      let () = Printf.printf "%s" "Match again" in 
      List.fold_left elabCase ve ls_case
  | Texp_try (_,ls_case) ->
      List.fold_left elabCase ve ls_case

  | Texp_tuple (expl) ->
      List.fold_left elabExpr ve expl
  | _ -> ve  


and elabValueBind ve {vb_pat;vb_expr;_ }= 
  let pat_desc = vb_pat.pat_desc in 
  (*see if pattern is [] or :: *)
  let consName_args_pair = match pat_desc with 
    | Tpat_construct (_, cons_desc, plist) ->
        let () = Printf.printf "%s" "&&\n" in 
        Some (cons_desc.cstr_name, cons_desc.cstr_args)

    | _ -> 
        let () = Printf.printf "%s" ">>\n" in 
        None  

  in 
  match consName_args_pair with 
  | None -> elabExpr ve vb_expr

  | Some (consName,args)->
      let () = Printf.printf "%s" ("consname::::::::::::::;"^consName) in  

      let cons_name_rel_lang = if consName = "[]" then "Nil" else (if consName = "::" then "Cons" else consName) in   
      let destTyD = vb_expr.exp_type in 
      let noramldestTyD = TyD.normalizeTypes destTyD in 
      let vid = Var.fromString ( cons_name_rel_lang ) in 
      let conTyD = match args with 
        | [] -> noramldestTyD 
        | argList -> 
          let tyDList = List.map (TyD.normalizeTypes) argList in 
          TyD.makeTarrow ((TyD.Ttuple tyDList), noramldestTyD) in 
      let conTyS =  toRefTyS (RefTy.fromTyD conTyD) 
      in 
      VE.add ve (vid,conTyS)





and  elabCase ve  {c_lhs;c_rhs;_} =
  let pat_desc = c_lhs.pat_desc in 
  let vb_expr= c_rhs in 
  (*see if pattern is [] or :: *)
  let consName_args_pair = match pat_desc with 
    | Tpat_construct (_, cons_desc, plist) ->
        let () = Printf.printf "%s" "&&\n" in 
        Some (cons_desc.cstr_name, cons_desc.cstr_args)

    | _ -> None


  in 
  match consName_args_pair with 
  | None -> elabExpr ve vb_expr

  | Some (consName,args)->
      let () = Printf.printf "%s" ("consname::::::::::::::;"^consName) in  
      let cons_name_rel_lang = if consName = "[]" then "Nil" else (if consName = "::" then "Cons" else consName) in   
      let destTyD = vb_expr.exp_type in 
      let noramldestTyD = TyD.normalizeTypes destTyD in 
      let vid = Var.fromString ( cons_name_rel_lang ) in 
      let conTyD = match args with 
        | [] -> noramldestTyD  
        | argList -> let tyDList = List.map (TyD.normalizeTypes) argList in 
          TyD.makeTarrow ((TyD.Ttuple tyDList), noramldestTyD) in 
      let conTyS = toRefTyS (RefTy.fromTyD conTyD) 
      in 
      VE.add ve (vid,conTyS)

open RefTy

let unifyConArgs (ve : VE.t) (con : Con.t) (vars : Var.t list) = 
  let conStr = Con.toString con in 
    let () = Printf.printf "%s" ("@here4 "^conStr) in 
                                                
  let convid = Var.fromString conStr in 
  let lenstr = fun v -> (string_of_int << List.length) v in 

  let conTy = try 
      RefTyS.specializeRefTy (VE.find ve convid)    
    with
    | _ -> raise (ElebEnvFail "Could not find constructor")
  in 
    let () = Printf.printf "%s" ("@here5 "^(RefTy.toString conTy)) in 
                                                
  match conTy with 
    Base _ -> let () =  assert(List.length vars = 0) in   
      []
  | Arrow ((argv,Base (_,argTyD,_)),Base (_,datTyD,_)) -> 
        let () = Printf.printf "%s" ("@here6 ") in 
  
      let () = assert (Vector.length vars = 1) in  
      List.map (fun (var) -> 
          (argv, var, argTyD, TyD.sametype argTyD datTyD)) vars

  | Arrow ((argv,Tuple tv), Base (_,datTyD,_)) ->
    let () = Printf.printf "%s" ("@here7 ") in 
      let () = assert (Vector.length tv = Vector.length vars) in 
    let () = Printf.printf "%s" ("@here8 ") in 
      
      let x = (List.map2 (fun (fldv, Base (_,argTyD,_)) var ->
          (newLongVar (argv,fldv), var, argTyD, 
           TyD.sametype argTyD datTyD)) tv vars) in 
      let () = Printf.printf "%s" ("@here9 ") in 
      x 
      
  | _ -> raise (ElebEnvFail "Could not unify and determine rec args")


let addRelToConTy (ve: VE.t) (con,letop,rexpr) (id:RelId.t) =
  let convid = Var.fromString (Con.toString con) in 
  let substs = match letop with 
      None -> []
    | Some lets -> List.map (fun (cvar,var,_,_) -> (cvar,var))
                     (unifyConArgs ve con lets) in 
  let rexpr' = RelLang.applySubsts substs rexpr in 
  let  conRefTys = 
    try VE.find  ve convid
    with 
    |_ -> raise (ElebEnvFail "Could not find constructor") 
  in 
  let RefTyS.T {tyvars;_} = conRefTys in 
  let targs = Vector.map (tyvars,TyD.makeTvar) in 
  let refty = RefTyS.specializeRefTy conRefTys in 
  let newref = fun var -> RP.Eq (RelLang.appR(id,targs,var),rexpr') in 
  let annotConTy = match refty with 
      RefTy.Base (bv,tyd,pred) -> RefTy.Base (bv,tyd, 
                                              Predicate.conjR (pred,newref bv))
    | RefTy.Arrow (arg,RefTy.Base (bv,tyd,pred)) -> RefTy.Arrow(arg,
                                                                RefTy.Base (bv,tyd, Predicate.conjR (pred,newref bv)))
    | _ -> raise (ElebEnvFail "Constructor type is neither base not arrow") in 
  let newTyS = RefTyS.generalizeRefTy (tyvars,annotConTy) 
  in
  VE.add (VE.remove ve convid) (convid,newTyS)


  (*
   * Synthesizes the type of rexpr in the given relational env.
   * Rel Env is constructed during elaboration, hence this function
   * is also part of elaboration.
   *)


exception CantInferType of string
exception Return of TS.cs list * TS.t * RelLang.instexpr

(* let testfun (tyDB) = 

   let () = let foo = fun x -> x + 1 in
        foo tyDB  
   in 
    ()

*)   
(*
  This is where relational expressions are translated to RefTy expressions
*)  
let rec elabRExpr (re,pre,tyDB,spsB,rexpr) =


  let typeSynthRElem elem = match elem with
    (*What should be the type constructor for int and bool*)
      RelLang.Int i -> TyD.makeTint()
    | RelLang.Bool b -> TyD.makeTbool ()
    | RelLang.Var v -> 
        try 
          TyDBinds.find (tyDB) (v)  
        with 
        | (TyDBinds.KeyNotFound _) -> raise (CantInferType "Type inference failed") in 
  
  let () = Printf.printf "%s" "@here-elabRE-1  \n" in 
                                           
  let doIt (e1,e2) cons f = 
    let (cs1,tupTy1,e1') = elabRExpr (re,pre,tyDB,spsB,e1) in 
    let (cs2,tupTy2,e2') = elabRExpr (re,pre,tyDB,spsB,e2) in 
    let (cs,tupTy) = f (tupTy1,tupTy2)
    in
    (mergecs [cs1;cs2;cs], tupTy, cons (e1',e2'))  in 

  let () = Printf.printf "%s" "@here-elabRE-2  \n" in 
            
  let isParam = fun rid -> SPSB.mem spsB rid in 
  (*This has some syntax error *) 
    let () = Printf.printf "%s" "@here-elabRE-3  \n" in 
 
    let doItParamApp ((RelLang.RInst {rel=rid;_} as rinst) ,tyd) = 
      let open SPSBValue in 
      let () = Printf.printf "%s" ("@here-elabRE-4   \n") in 
      let {dom;range=svar} = SPSB.find spsB rid in 
      let _ = match !dom with 
        None -> 
        let () = Printf.printf "%s" ("@here-elabRE-4-1  \n") in 
        dom := Some tyd
      | Some tyd' -> 
          let res = TyD.sametype tyd tyd' in
          let () = Printf.printf "%s" ("@here-elabRE-4"^(string_of_bool res)^"  \n") in 
 
          assert (res) in 

 
    let expsort = (TS.fromSVar svar) 
    in 
    let () = Printf.printf "%s" "@elabRE-exit  \n" in 
                                           
    (emptycs(), expsort, rinst) 

  in 


  let doItPrimApp (RelLang.RInst {rel;args;_} ,tyd) =
    let open PRE in 
    let open RelLang in 
    let relName = RelId.toString rel in 
    let argRels = List.map (fun (RelLang.RInst {rel;_}) -> rel) args in 
    let  argTyDs =  List.map (
        fun r -> 
          let var = r in 
          TyDB.find tyDB var     
      ) argRels in 
          (*
           * All arguments of primitive relation are of polymorphic
           * type
           *)
    let targs = List.concat[argTyDs ; Vector.new1 tyd] in 
    let {tys;def} = PRE.find pre rel in 
    let relTyS = tys in 
    let relSS = PTS.instantiate (relTyS,targs) in 
    let PSS.T {sort=ProjSort.T {sort; _}; _} = relSS in 
    let SPS.ColonArrow (prD,prR) = sort in 
    let expectedPrD = List.fold_left (fun x y -> TyD.makeTarrow (x,y)) (tyd) (argTyDs) in 
    let err = fun _ -> relName^" application failed typecheck.\n" 
                       ^ "Expected: " ^ (TyD.toString expectedPrD)^"\n" 
                       ^ "Got: "^(TyD.toString prD)^"\n" in 
    let _ = assert (TyD.sametype expectedPrD prD) in 

    let newRInst = RelLang.RInst {rel=rel;args=args;targs= (targs);
                                  sargs=empty}
    in
    (emptycs(), prR, newRInst) in 

  let open RelLang in 
  
  let rec doItRInstApp ((RInst {rel;args;_} as rinst) ,tyd) = 
        let open ParamRelEnv in 

        let _ = if isParam rel 
          then
            let (x,y,z) =  doItParamApp (rinst,tyd) in 
            raise (Return (x,y,z))
          else () in 
        let relName = RelId.toString rel in 

        let () = Printf.printf "%s" ("doItRInstApp-1"^relName) in   
        let {tys=relTyS;def} =           
          try (PRE.find pre rel) with

          | PRE.ParamRelNotFound _ -> raise (ElebEnvFail ("Inst of unknown\
                                                          \ prim/param relation " ^relName)) in 


        let _ = match def with 
            PRE.Prim _ -> 
              let (x,y,z) = doItPrimApp (rinst,tyd) in 
              raise (Return (x,y,z))
          (* Hack: For recursive applications. *)
          | PRE.Bind Bind.BogusDef -> (raise (ElebEnvFail ("Cant infer type")))
          | _ -> () in 
        let tyd' = PTS.domain relTyS in 
        let () = Printf.printf "%s" ("doItRInstApp-2"^(TyD.toString tyd')) in   
        

        let targs = 
          let tydcomp =  tyd  in 
          
          match (tydcomp,tyd') with 
            (TyD.Tconstr (tycon1, targs), TyD.Tconstr (tycon2,_)) ->
                let () = Printf.printf "%s" ((TyD.toString tydcomp)^"\n") in 
                let () = Printf.printf "%s" ((TyD.toString tyd')^"\n") in 
                
                assert  (Tycon.equals (tycon1, tycon2))  ; targs
            | (_,TyD.Tvar _) -> [tyd]
            | _ -> raise (ElebEnvFail ("RelApp type mismatch: " ^ relName) ) in  

        
        let relSS = PTS.instantiate (relTyS,targs) in 

        let () = Printf.printf "%s" ("@here-elabRE-5 \n") in 
        
        let PSS.T {sort=ProjSort.T {paramsorts;_}; _} = relSS  in 

        let argDomains = List.map (fun (SPS.ColonArrow (argTyd,_)) -> argTyd) paramsorts in 

        let _ = assert (Vector.length args = Vector.length
                          argDomains) in 
        let (argRanges, args') = Vector.unzip  (List.map2 
                                                  (fun arg argD ->
                                                     let (_,sort,arg') = doItRInstApp (arg,argD)
                                                     in (sort,arg')) args argDomains) in  
       (*
        * sargs are instantiations for sort vars in param ranges.
        * Since an arg instantiates param, sargs=argRanges
        *)
        let sargs = argRanges in 
        let ProjSort.T {paramsorts; sort = SPS.ColonArrow (_,expsort)} = 
          PSS.instantiate (relSS,sargs) in 
        let open Types in    
        let newRInst = RInst {rel=rel; args=args'; targs=targs;
                              sargs=sargs}
        in

        (emptycs(), expsort, newRInst)   

         in    

    let doItRInstApp = fun (rinst, x) ->
        let tyd_found =  TyDB.find tyDB x in   
        let (cs,sort,rinst') = doItRInstApp (rinst, tyd_found)
        in
        (cs,sort,R (rinst',x)) 

  in 
  (*function for the constructors as const can't be used as functions in OCAML*)
  let open RelLang in 
  let funU (e1, e2) = U (e1, e2) in 
  let funX (e1, e2) = X (e1, e2) in 
  let funD (e1, e2) = D (e1, e2) in 
  let funT td = TS.T td in
  let () = Printf.printf "%s" ("["^(exprToString rexpr)^"]") in 
  match rexpr with
    U (v1, v2) -> doIt (v1,v2) (funU) TS.unionType 
  | X (v1,v2) -> doIt (v1,v2) (funX) TS.crossPrdType
  | D (v1,v2) -> doIt (v1,v2) (funD) TS.unionType 
  | T els -> (emptycs(), TS.Tuple   
                (List.map (funT << typeSynthRElem) els) ,rexpr)
  | R (rinst,y) ->  
   let () = Printf.printf "%s" ("@here-elabRE-6 \n") in 
     
    doItRInstApp (rinst,y)  



let  elabPRBind (pre) (PR.T {id;def}) =
  (*This creates a new Tyvar without a name, Where is this used ??*)
  let newVarTyD = fun _ -> TyD.makeTvar (Ident.create "") in 
  let rec bindVars tyDB (PR.Nary (v,def)) = bindVars (TyDB.add tyDB  v ( newVarTyD())) def  in
  let bindVars tyDB (PR.Nullary rexpr) = (tyDB,rexpr) in 

  let  (tyDB, rexpr) = bindVars TyDB.empty def in 
    
  let (_,prRange,rexpr') = elabRExpr (RE.empty, pre, tyDB, 
                                      SPSB.empty, rexpr) in 
  let rec doItDef =  function 
    |(PR.Nary (v,def)) -> 
        let (TyD.Tvar tyvar) as varTyD  = TyDB.find (tyDB) (v) in 
        let (tyvars, prDomOp, def') = doItDef def in 
        let prDom = match prDomOp with 
            None -> varTyD
          | Some tyd -> TyD.makeTarrow (varTyD, tyd) in 
        let  prDef = PR.Nary (v,def') in 

        (tyvar::tyvars, Some prDom, prDef)

    | (PR.Nullary _) -> ([],None,PR.Nullary rexpr') in 

  let (tyvars, Some prDom,prDef) = doItDef def in 
  let prSPS = SPS.ColonArrow (prDom,prRange) in 
  let prSS = PSS.T {svars=empty(); 
                    sort = ProjSort.T {paramsorts=empty(); sort=prSPS}} in 
  let prTS = PTS.T {tyvars=  tyvars; 
                    sortscheme=prSS} in 
  let  reldesc = {PRE.tys=prTS; PRE.def= PRE.Prim prDef}
  in
  PRE.add pre (id,reldesc)

let elabSRBind (re)(pre)(ve ) (StructuralRelation.T {id;params;mapp}) =

  let open SPSBValue in 
  let open SPSBKey in   



  let spsB = List.fold_left (fun spsB r -> 
      SPSBinds.add spsB r {dom = ref None; range = SVar.newSVar ()}) SPSBinds.empty params in 
  let isParam = fun rid -> SPSB.mem spsB rid in 

  (* First pass - type & sort annotate instantiations *)

  let () = Printf.printf "%s" "@here1 \n" in 
  let (map', relTySOp) = Vector.mapAndFold (mapp, None, 

                                            fun ((con,valop,rterm), relTySOp) ->
                                              match (valop ,rterm) with
                                                (None,RelLang.Expr _) -> (* must be Rnull *)
                                                  ((con, valop  , rterm), relTySOp) 
                                              | (None,RelLang.Star ie) ->
                                                  let  _ = match relTySOp with
                                                      None -> ()
                                                    | Some _ -> raise (ElebEnvFail "Ind uasge wrong") in 

                                                  let RelLang.RInst {rel;args;_} = ie in 
                                                  let  argRels = List.map (fun (RelLang.RInst {rel;_}) -> rel) args in 
                                                  let  _ = assert (Vector.forall (argRels, isParam)) in 
                                                  let open PRE in 
                                                  let {tys=relTyS;_} = 
                                                    try 
                                                      PRE.find pre rel 
                                                    with
                                                    | PRE.ParamRelNotFound _ -> raise (ElebEnvFail "Id of unknown param relation")
                                                  in 


                                                  let  PTS.T {tyvars;sortscheme = relSS} = relTyS in 
                                                  let targs = Vector.map (tyvars, 
                                                                            TyD.makeTvar) in 
                                                  let PSS.T {svars;sort} = relSS in
                                                  let sargs = Vector.map (svars, TS.fromSVar) in 
                                                  let newRInst = RelLang.RInst {rel=rel; args=args; 
                                                                                targs=targs; sargs=sargs}
                                                  in
                                                  ((con, valop , RelLang.Star newRInst), Some relTyS)

                                              | (Some vars, RelLang.Expr rexpr) -> 
                                                  let () = Printf.printf "%s" "@here3 \n " in 
                                                  let convid = Var.fromString (Con.toString con) in 
                                                  let RefTyS.T {tyvars;refss;_} = 
                                                    try VE.find ve convid 
                                                    with 
                                                    | VE.VarNotFound _ -> 
                                                        let s = "Constructor " ^(Con.toString con)^ " not found in var env." in 
                                                        raise (ElebEnvFail s) in 
                                                  let () = Printf.printf "%s" "@here3-1  \n" in 
                                                
                                                  let refty = RefSS.toRefTy refss in
                                                  let datTyD = match refty with
                                                      RefTy.Base (_,datTyD,_) -> datTyD
                                                    | RefTy.Arrow (_,RefTy.Base (_,datTyD,_)) -> datTyD
                                                    | _ -> raise (ElebEnvFail "Impossible case") in 
                                                  let () = Printf.printf "%s" "@here3-2  \n" in 
                                                  
                                                  let tyDB = List.fold_left (fun tyDB (_,var,tyD,_) -> TyDBinds.add tyDB var tyD)  
                                                      TyDBinds.empty (unifyConArgs ve con vars)in 

                                                   let () = Printf.printf "%s" "@here3-3  \n" in 
                                                       
            (*
             * Hack : For structural relations with recursive
             * occurances, we currently assume existence of a base
             * case (with non-empty RHS) in order to infer its sort.
             * We extend PRE with binding for current relation (id),
             * mapping it to a bogus def. This is to identify
             * recursive applications.
             *)
                                                  let open PRE in  
                                                  let bogusDesc = {
                                                    tys = PTS.simple (empty, SPS.ColonArrow
                                                                        (*author Ashish : Treating Tnil as Tunknown in SML , confirm ??*)
                                                                        (TyD.Tunknown,TS.Tuple []));
                                                    def = PRE.Bind ( Bind.BogusDef)} in

                                                  let extendedPRE = PRE.add pre (id,bogusDesc) in 
                                                  let () = Printf.printf "%s" "@here3-4  \n" in 
                                           
                                                  let (cs,tupTy,rexpr') = elabRExpr (re, extendedPRE, (tyDB), spsB, rexpr) in 
                                                  let () = Printf.printf "%s" "@here3-5  \n" in 
                                           
                                                  let _ = assertEmptyCs cs in 
                                                  let relSPS = SPS.ColonArrow (datTyD, tupTy) in 
                                                  let (svars, paramSPS) = Vector.unzip (Vector.map 
                                                                                          (spsB, fun (_,{dom;range=svar}) -> 
                                                                                              let tyD = match !dom with
                                                                                                  Some tyD -> tyD
                                                                                                | None -> raise (ElebEnvFail "Unused rel param\n") in 
                                                                                              let tupTy = TupSort.fromSVar svar
                                                                                              in
                                                                                              (svar,SPS.ColonArrow (tyD,tupTy))
                                                                                          )) in 
                                                
                                                  let relPS = ProjSort.newProjSort paramSPS relSPS in 
                                                  let relSS = PSS.generalize (svars,relPS) in 
                                                  let relTyS = PTS.generalize (tyvars,relSS) 
                                                  in 

                                                  ((con, valop  , RelLang.Expr rexpr'), Some relTyS)

                                              | _ -> raise (ElebEnvFail "Impossible case of valop -rterm :") ) in 


  let () = Printf.printf "%s" "@here2 " in 
  let pts = match relTySOp with
      None -> raise (ElebEnvFail "Failed Elaboraion")
    | Some relTyS -> relTyS in 
  let  bdef = Bind.makeBindDef (id,params,pts) in 
  let open PRE in 
  let  pre' = PRE.add pre (id,{tys=pts;def = PRE.Bind bdef}) in

  (* Second pass - make ground def; expand inductive defs*)

  let map'' = List.concat  
      (Vector.map (map', 
                   fun (con,valop ,rterm) ->
                     match Bind.makeGroundDef (params,rterm) with

                       RelLang.Expr rexpr -> [(con,valop ,rexpr)]
                     | RelLang.Star (RelLang.RInst {rel = relId;_}) -> 
                         let open RE in 
                         let  
                           {ty=relTyS;map} = 
                           try RE.find re relId with
                           | (RE.RelNotFound r) -> raise (ElebEnvFail 
                                                            ("Ind of unknown ground rel : "^(RelId.toString r))) in 

                         let PTS.T {tyvars;_} = relTyS in 
                         let open SpecLang in  
                         let targs = Vector.map (tyvars,
                                                    TyD.makeTvar )


                         in
                         Vector.map (map, 
                                     fun (con,valop ,rexpr) -> match valop  with 
                                         None -> (con,valop ,rexpr)
                                       | Some vars -> 
                                           let partions = List.partition (fun (_,_,_,isrec) -> isrec) (unifyConArgs ve con vars)  
                                           in 
                                           let frst = fst partions in     
                                           let recvars = List.map (fun (cvar,_,_,_) -> cvar) frst in 
                                           let recRApps = Vector.map (recvars, fun var -> 
                                               RelLang.appR (id,targs,var)) in 
                                           let recRAppsUnion = 
                                             List.fold_left (fun e1 e2 -> (RelLang.union (e1, e2))) (RelLang.rNull()) (recRApps) in 
                                           let rexpr' = RelLang.union (rexpr, recRAppsUnion)
                                           in
                                           (con,valop ,rexpr')
                                    )
                  )
      )
  in           
  let ty' = Bind.groundRelTyS pts in 
  let  re' = RE.add re (id,{ty=ty';map=map''}) 
  in
  (re',pre')

(*
   * Produces a refTy' with base types taken from TyD and
   * refinements from refTy, given that user-provided base
   * types in refTy are unifiable with base types in tyd
   * Caution : tyvar unification is not uniform. 
   *)  
let mergeTypes (tyd , refTy ) =
  let open Types in
  let open TyD in  
  let open RefTy in 

  let mergeErrorMsg (tyd,tyd') = "Cannot merge ML type " ^ (TyD.toString tyd)
                                 ^ " with type given in spec: " ^ (TyD.toString tyd') in 

  let rec doMerge (tyd) ((argv, refTy) as argBind) = 
    match (tyd,refTy) with
    |   (_,Base (bv,tyd',pred)) -> 
        let _ = assert (unifiable (tyd,tyd')) in 
        let newArgBind = (argv, Base (bv,tyd,pred))
        in
        (Vector.new0 (), newArgBind)

        (*Look at the merging operation again *)
   (* |  ( TyD.Ttuple telist , Tuple argBinds') ->
        
        let (substss,newArgBinds') = (Vector.unzip 
                                        ( List.map2 
                                            (fun (lbl) ((argv',refty') as argBind' ) -> 
                                               let newargv' = Var.fromString (lbl) in 
                                               let (substs,newArgBind) = doMerge tyd (newargv',refty') in 
                                               let substs = Vector.concat [Vector.new1 (newargv',argv'); 
                                                                           substs]  in 
                                               (substs,newArgBind)) (te_list) (argBinds'))  

                                     ) in
        let substs = Vector.map (List.concat substss,
                                 fun (n,ol) ->  (newLongVar (argv,n), ol)) 
        in 
        let newArgBind = (argv, Tuple newArgBinds')
        in
        (substs, newArgBind)

 *)
    | ( TyD.Tarrow (tyd1,tyd2), Arrow (argBind,resTy) ) ->
        let (substs,argBind') = doMerge tyd1 argBind in 
        let dummyArgVar = argv in 
        let (_,(_,resTy')) = doMerge tyd2 (dummyArgVar, 
                                           RefTy.applySubsts substs resTy) in 
        let newArgBind = (argv, Arrow (argBind',resTy'))
        in
        (Vector.new0 (), newArgBind)

    | _ -> raise (ElebEnvFail ("Types Merge Error. Cannot merge\n"
                               ^ "1. "^(L.toString $ RefTy.layout refTy)^", \n"
                               ^ "2. "^(TyD.toString tyd)^"\n") ) in 
  let (_,(_,refTy')) = doMerge tyd (genVar (), refTy)
  in
  refTy'


    (*
   * Forall top-level fun decs, elabDecs annotates their ML
   * types with type refinements. 
   *)

(*In all the example I see Texp_fun has a single case i.e. pat -> exp)*)
open Typedtree 
let get_function_args_wType (case_list) =

  List.map (fun cs ->
      let arg_cs = cs.c_lhs in 
      let argty_cs = cs.c_lhs.pat_type in 
      (arg_cs, argty_cs)    
    ) case_list


let get_name_for_pat p = 
  let p_desc = p.pat_desc in 
  match p_desc with 
    Tpat_var (id, _) -> id
  |_ -> genVar()



let elab_vbs (ve) (vb_list) = 
  let elab_vb ve vb = 
    let vb_pat = vb.vb_pat in 
    let vb_exp = vb.vb_expr in 
    match vb_exp.exp_desc with 
    | Texp_function (_, case_list,_) -> 

        let arg_type_list = get_function_args_wType (case_list) in   
        let (arg, argType) = List.nth arg_type_list 0 in 

        let body = 
          let case_list_first = List.nth case_list 0 in 
          case_list_first.c_rhs in 
        (*The original sml version has toMyType which we do not need*)
        let normalargTyD = TyD.normalizeTypes argType in 
        let normalbodyTyD = TyD.normalizeTypes body.exp_type in 

        let funTyD = TyD.makeTarrow(normalargTyD, normalbodyTyD) in 
        let fun_name =get_name_for_pat vb_pat in
        let () = Printf.printf "%s" (Var.toString (fun_name)^"\n") in 
        let funTyS = VE.find ve fun_name in 



        let funRefTy = mergeTypes (funTyD, RefTyS.specializeRefTy 
                                     funTyS) in 
        (* skipping the parametric refinement types for now 
           let RefSS.T{svars; prefty= PRF}
        *)
        let RefSS.T{svars; prefty=PRf.T{params;_}} = 
          RefTyS.specialize funTyS in 

        let funRefSS = RefSS.T{svars=svars; prefty=PRf.T 
                                                {params=params; refty=funRefTy}} in 
        (*what are the type variables??*)   
        let funspec = RefTyS.generalizeAssump ([], funRefSS, RefTyS.isAssumption funTyS)
        in
        VE.add (VE.remove ve fun_name) (fun_name,funspec)

    | _-> ve in 
  List.fold_left (elab_vb) ve vb_list      


let elab_possible_fun_exp (ve, str_desc) = 

  let () =  Printf.printf "%s" "elab_possible_fun_exp" in 
  match str_desc  with
  |  Tstr_eval (exp,_) -> 
      let exp_desc = exp.exp_desc in 
      let exp_type = exp.exp_type in 
      let extendedve = match exp_desc with 
        | Texp_let (_, vb_list, _) -> elab_vbs ve (vb_list)  
        | _-> ve in 
      extendedve

  | Tstr_value (_, vb_list) -> elab_vbs ve (vb_list)

  | _ -> ve


let elab_structure_items (ve ,str_items) =
  let elab_str_item ve str_item  = 
    match str_item.str_desc with
    | Tstr_value (_, lvb) -> elab_possible_fun_exp (ve, str_item.str_desc) 
    | Tstr_eval (exp, attr)-> elab_possible_fun_exp (ve, str_item.str_desc) 

    | _ -> ve in 
  let  extendedVE = List.fold_left (fun ve str_item  -> elab_str_item ve str_item) ve str_items  
  in
  extendedVE


let elabTypeSpec re pre  (ParamRefType.T{params;refty}) =
  let open SPSBValue in
  let open SPSBKey in  
  (* Initially, param domains are none *)
  let spsB = List.fold_right (fun r spsB -> 
      SPSBinds.add spsB (fst r) {dom = ref None;range = SVar.newSVar ()}) params SPSBinds.empty  in 
  let isParam = fun rid -> SPSBinds.mem spsB rid in 

  let doItRelPred tyDB rp = 
    let open RP in
    let doIt (r1,r2) cons =
      let (cs,_,RelLang.U (r1',r2')) = elabRExpr
          (re,pre,tyDB,spsB, RelLang.U (r1,r2)) in 

      (cs, cons (r1',r2'))
    in

    match (rp) with 
      Eq (x1, x2) -> doIt (x1,x2) (fun (x1, x2) -> Eq (x1, x2)) 
    | Sub (x1, x2) -> doIt (x1,x2) (fun (x1, x2) -> Sub (x1, x2))
    | SubEq (x1, x2) -> doIt (x1,x2) (fun (x1, x2) -> SubEq (x1, x2))in 

  let rec doItPhi tyDB phi = 
    let doItTup (p1,p2) cons =
      let (cs1,p1') = doItPhi tyDB p1 in 
      let (cs2,p2') = doItPhi tyDB p2 in 
      let cs = mergecs [cs1;cs2]
      in
      (cs,cons (p1',p2'))

    in
    match phi with
    |  P.Conj (p1,p2) -> doItTup (p1,p2) (fun (p1,p2) -> P.Conj (p1,p2)) 
    | P.Disj (p1,p2) -> doItTup (p1,p2) (fun (p1,p2) -> P.Disj (p1,p2)) 
    | P.If  (p1,p2) -> doItTup (p1,p2) (fun (p1,p2) -> P.If(p1,p2))  
    | P.Iff  (p1,p2) -> doItTup (p1,p2) (fun (p1,p2) -> P.Iff (p1,p2)) 
    | P.Not t -> (fun (cs,t') -> (cs,P.Not t')) ( doItPhi tyDB t )
    | P.Base _ -> (emptycs(),phi)
    | P.Rel rp -> (fun (cs,rp') -> (cs, P.Rel rp')) (doItRelPred tyDB rp)
    | _ -> (emptycs(),phi) in 




  let mapFst = fun f -> fun (x,y) -> (f x,y) in 
  let inv = fun (x,y) -> (y,x) in 

  let rec doItRefTy tyDB refty = match refty with 
      RefTy.Base (bv,tyd,phi) -> 
        let
          (cs,phi') = doItPhi (TyDB.add tyDB bv tyd) phi
        in
        (cs, RefTy.Base (bv,tyd,phi'))

    | RefTy.Arrow ((x,t1),t2) -> 
        let (cs1,t1') = doItRefTy tyDB t1 in 
        let  tybinds = match t1 with 
            RefTy.Tuple _ -> RefTy.decomposeTupleBind (x,t1)
          | _ -> Vector.new1 (x,t1) in 
        let  tydbinds = Vector.map (tybinds, fun (v,ty) ->
            (v,RefTy.toTyD ty)) in 
        let  tyDB' = List.fold_left (fun tyDB (x,tyd) -> TyDB.add tyDB x tyd) tyDB tydbinds in 
        let  (cs2,t2') = doItRefTy tyDB' t2
        in
        (List.concat [cs1;cs2], RefTy.Arrow ((x,t1'),t2'))

    | RefTy.Tuple vts -> 
        let temp = ( 
          Vector.mapAndFold (vts, emptycs(), 
                             fun ((v,t),csAcc) -> 
                               let  (cs,t') = doItRefTy tyDB t in 
                               let  cs' = List.concat [cs;csAcc] in 
                               ((v,t'),cs')
                            )
        ) in 
        inv ( mapFst (fun x -> RefTy.Tuple x) temp)  in 
  let  (cs,refty') = doItRefTy TyDB.empty refty in 
  let  (solfn :SVar.t -> TS.t) = solvecs cs in 
  let  sortedParams = Vector.map (SPSB.toVector spsB, 
                                  fun (r,{dom;range=svar}) -> 
                                    match  !dom with 
                                      Some tyd -> (r,SPS.ColonArrow (tyd,solfn svar))
                                    | None -> raise (ElebEnvFail "Unused rel param:")) in 

  (* let  {unions;empty;_} = List.set {equals=SVar.eq; 
     layout = fun _ -> L.empty} in 
  *)
  let  svars = []  (* List.fold_left (fun ((_,SPS.ColonArrow (_,tupTy)) ) s  ->
                      let ls_tupTy = match tupTy with 
                      RefTy.Tuple tTl -> tTl 
                      | _ -> [] in 
                      let svarsin_tupTy = TS.getSVars tupTy in
                      List.concat[svarsin_tupTy ; s]) sortedParams [] *) in 
      (*
       * solfn has to be applied to sort arguments in refty.
       *)
  let  prf = PRf.parametrize (sortedParams, 
                              RefTy.mapSVar refty' solfn) in 
  let  refSS = RefSS.generalize (svars,prf)
  in
  refSS



let elaborate (tstr) (RelSpec.T {reldecs;primdecs; 
                                 typespecs}) =

  let tstr_items = tstr.str_items in
  let veWithBool = bootStrapBools VE.empty in 


  let  _ = Printf.printf "@Var Env Before:\n" in
  let  _ = Printf.printf "%s" ((VE.layout veWithBool)) in


  let initialVE = List.fold_left
      (fun ve str_item ->
         let str_item_desc = str_item.str_desc in 
         match str_item_desc with 
         | Tstr_value (flag, vbl) -> 
             List.fold_left (fun ve vb -> elabValueBind ve vb) ve vbl 
         | Tstr_eval (expr, attr) ->
             elabExpr ve expr
         |_ -> ve) veWithBool tstr_items in 


  let  _ = Printf.printf "@Var Env After:\n" in
  let  _ = Printf.printf "%s" ((VE.layout initialVE)) in


  let  initialPRE = List.fold_left (fun (pre) (primbind) -> elabPRBind pre primbind) PRE.empty  primdecs in 

  let  _ = Printf.printf "@PRE Env After:\n" in
  let  _ = Printf.printf "%s" (Layout.toString (PRE.layout initialPRE)) in

  

  let  initialRE =  RE.empty in 
  let  (elabRE,elabPRE) = List.fold_left (fun (re, pre) srbind -> 
     elabSRBind re pre initialVE srbind) (initialRE, initialPRE) reldecs  in 

  let  _ = Printf.printf "@RE Env After:\n" in
  let  _ = Printf.printf "%s" (Layout.toString (RE.layout initialRE)) in


  let  refinedVE = Vector.fold (RE.toVector elabRE, initialVE, 
                                fun ((id,{ty;map}),ve) -> Vector.fold (map, ve, 
                                                                       fun (conPatBind,ve) -> addRelToConTy ve conPatBind id)) in 

  let refinedVE = initialVE in 
  let  protoVE = List.fold_left          
      (fun ve (TypeSpec.T {isAssume;name;params;refty}) -> 
         let  dummySPS = 
           SPS.ColonArrow ( TyD.makeTvar (Tyvar.fromString (Var.toString (genVar ())))
                          , TS.Tuple 
                              [TS.S ( SVar.newSVar())]) in 
         let  sortedParams = Vector.map (params, 
                                         fun r -> (r,dummySPS)) in 
         let  prefTy = PRf.parametrize (sortedParams,refty) in
         let  refSS = RefSS.generalize (empty(),prefTy) in
         let  refTyS = RefTyS.generalizeAssump
             (empty(),refSS,isAssume)
         in
         VE.add ve (name,refTyS)
      ) VE.empty  typespecs 
  in  
  (*defined elab_structure_items in place of elabDecs for ocaml*)
  let  typedVE = elab_structure_items (protoVE, tstr_items) in 

  let  fullVE = List.fold_left  
      (fun  (ve) (name,RefTyS.T {tyvars;isAssume;refss}) ->
         let  RefSS.T {prefty;_} = refss in 
         let  PRf.T {params=sortedParams;refty} = prefty in 
         let  (params,_) = Vector.unzip sortedParams in 
         let  refss' = elabTypeSpec (elabRE) (elabPRE) prefty in 
         let  refTyS = RefTyS.T {tyvars=tyvars;isAssume=isAssume; 
                                 refss=refss'} 
         in
         VE.add ve (name,refTyS)
      ) typedVE refinedVE
  in
  (fullVE,elabRE,elabPRE)
