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

(*comment this out to print  Printf messages*)
module Printf = struct
  let printf f s = ()

end 


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

let count = ref 0
let  getUniqueId symbase =
  let id = symbase ^ (string_of_int(!count)) in 
  let _ = count := !count + 1 in 
  Var.fromString id 

let genVar () =  getUniqueId "x_" 
let newLongVar = fun (var,fld) -> Var.fromString (
                                  (Var.toString var)^"."^(Var.toString fld))
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



let toRefTyS refTy = RefTyS.generalize (Vector.new0(), 
                                        RefSS.fromRefTy refTy)

let bootStrapBools ve = 
  let boolTyD = TyD.makeTbool () in 
  let tvid = Var.fromString (Con.toString Con.truee) in 
  let  fvid = Var.fromString ( Con.toString Con.falsee) in 
  let RefTy.Base (v1,t,_) = RefTy.fromTyD boolTyD in
  let RefTy.Base (v2,t,_) = RefTy.fromTyD boolTyD in
   
  let eqPred1 = P.baseP ( BP.varBoolEq (v1,true) ) in 
  let eqPred2 = P.baseP (  BP.varBoolEq (v2,false) ) in 
  let empty = Vector.new0 () in
  let tTyS = RefTyS.generalizeRefTy (empty, 
                                     RefTy.Base (v1,t,eqPred1)) in 
  let fTyS = RefTyS.generalizeRefTy (empty, 
                                     RefTy.Base (v2,t,eqPred2)) in
  let ve' = VE.add ve (tvid,tTyS) in 
  let ve'' = VE.add ve' (fvid,fTyS) in 
  let ve'' = VE.add ve'' (v1, toRefTyS (RefTy.fromTyD boolTyD)) in 
  let ve'' = VE.add ve'' (v2, toRefTyS (RefTy.fromTyD boolTyD))
  in
  ve''
let bootStrapCons ve = 
  (*create nil*)
  let nilvid = Var.fromString "[]" in 
  let nilcosntantvid = Var.fromString "nil" in 
  let consvid = Var.fromString "::" in 
  let conparam = TyD.Tvar (Tyvar.fromString "int") in 
  let list_cons = Tycon.fromString "list" in 
  let listconsparams = [conparam] in 
  let listTyD = TyD.makeTconstr (list_cons, listconsparams) in 


  let nilRefTy = RefTy.fromTyD listTyD in 
  let nilRefTyS = toRefTyS nilRefTy in 


  (*create cons*)
  let typex = (RefTy.Base (Var.fromString "x", conparam, Predicate.truee())) in 
  let typexs = (RefTy.Base (Var.fromString "xs", listTyD, Predicate.truee())) in 
  let paramTuple = [(Var.noName, RefTy.Base (Var.fromString "x", conparam, Predicate.truee())); 
                    (Var.noName, RefTy.Base (Var.fromString "xs", listTyD, Predicate.truee()))] in
  let consArgsRefTy = RefTy.Tuple (paramTuple) in  
  let consResRefTy = RefTy.fromTyD listTyD in 

  let consRefTy = RefTy.Arrow ((Var.noName, consArgsRefTy),  consResRefTy) in 

  let consRefTyS = toRefTyS consRefTy in 
  let ve' = VE.add ve (nilvid,nilRefTyS) in 
  let ve'' = VE.add ve' (consvid,consRefTyS) 
  
  in 
  let type_nil_constant = (* RefTy.fromTyD listTyD in *)
                          RefTy.Base (Var.fromString "[]", listTyD, Predicate.truee()) in 
  let typeS_nil_constant = toRefTyS type_nil_constant in  
  let ve''' = VE.add ve'' (nilcosntantvid, typeS_nil_constant) in 

  
  ve'''



  (*Unimple, the tyvar for the data type is empty for now*)
  let extract_param_tyvars corety_variance_list = 
    []
  (*type_declaration =
  {
    typ_id: Ident.t;
    typ_name: string loc;
    typ_params: (core_type * variance) list;
    typ_type: Types.type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_loc: Location.t;
    typ_attributes: attributes;
   }
    type binarytree = E of Tree | T of binarytree * binarytree 
      *)
  let elabDataTypeDecl ve (tdecl) = 
    let open Typedtree in 
    let type_id = tdecl.typ_id in
    let cons_for_type = Tycon.fromString (Ident.name type_id) in 
    (*get the TyD.Tvar for the tyvars*)
    let cons_type_paramsTyd_list = extract_param_tyvars (tdecl.typ_params) in 

    let destTyD = TyD.makeTconstr (Tycon.fromString (Ident.name (type_id)), cons_type_paramsTyd_list) in 
 
    let type_kind = tdecl.typ_kind in 
    match type_kind with 
      Ttype_abstract -> (*Unimpl*) ve 
      | Ttype_variant (cd_list) -> 
        
          (* type bt... = 
              | E of int
              | T of bt * bt
           *)
          (*constructor_declaration =
            {
             cd_id: Ident.t;
             cd_name: string loc;
             cd_args: constructor_arguments;
             cd_res: core_type option;
             cd_loc: Location.t;
             cd_attributes: attributes;
            }
        *)

          let ve' = List.fold_left (fun ve cd -> 
                let data_cons_id (*e.g. Node *) = cd.cd_id in 
                let argsRefTys = 
                  match cd.cd_args with 
                    | Cstr_tuple (core_type_list) ->
                        (*return the list of the TyD.t*)
                        let getTyd_core_type coretyp = 
                          let ctype_desc = coretyp.ctyp_desc in 
                          match ctype_desc with 
                            Ttyp_any -> TyD.Tunknown 
                            | Ttyp_var (s) -> TyD.makeTvar (Tyvar.fromString s)
                            | Ttyp_arrow (_,_,_)-> (*Unimpl*) raise (ElebEnvFail "Ttyp_arrow not handled as type variant argument") 
                            | Ttyp_tuple (ct_list) -> (*Unimpl*) raise (ElebEnvFail "Ttyp_tuple not handled as type variant argumen") 
                            | Ttyp_constr (p , l, ct_list) ->
                                 let arg_tyconsId = 
                                   if Tycon.is_ident p then
                                      Path.head p 
                                  else 
                                    raise (ElebEnvFail "Only Identities allows as paths")  

                                  in   
                                 (*In general scenario the arguments might have parameters again
                                 int passed to a data constructor is a Tcons in Ocaml*)   
                                 let argTyD = if ((Tycon.toString p ) = "int")
                                  then TyD.makeTvar (Tyvar.fromString "int")
                                  else 
                                  TyD.makeTconstr (Tycon.fromString (Ident.name arg_tyconsId) , []) 
                                in 
                                argTyD

                        in 
                        let args_basetypelist = List.map (getTyd_core_type) core_type_list in 
                        let args_RefinementTypeList = List.map (fun baseTy -> RefTy.fromTyD baseTy) args_basetypelist in 
                        args_RefinementTypeList  
                      


                    | Cstr_record (ld_list) -> (*Unimp*) raise (ElebEnvFail "User defined data-types with records as variant not handled")
                  in 

                 let refTy_data_cons = 
                  match argsRefTys with 
                    [] -> RefTy.fromTyD destTyD
                    | _ -> 
                      let args_var_refTyBind  =List.map (fun refTy -> (Var.noName, refTy)) argsRefTys in 
                      let cons_argsRefTuple = RefTy.Tuple (args_var_refTyBind) in   
                      RefTy.Arrow ((Var.noName, cons_argsRefTuple),  RefTy.fromTyD destTyD) 
                 in 
                  
                 let ve' = VE.add  ve (data_cons_id, toRefTyS refTy_data_cons) in 
                 ve'

                ) ve cd_list in 

              ve'

      | Ttype_record (ld_list) -> (*Unimpl*) raise (ElebEnvFail "Ttype_record not handled yet while elaborating a type decl")
      | Ttype_open -> (*Unimpl*) ve 


(*extension_constructor =
  {
    ext_id: Ident.t;
    ext_name: string loc;
    ext_type : Types.extension_constructor;
    ext_kind : extension_constructor_kind;
    ext_loc : Location.t;
    ext_attributes: attributes;
  }*)
let elabExceptionDecl ve ext_cons =
    let () = Printf.printf "%s" ("Exception") in  
    let open Typedtree in 
    let exception_name_loc = ext_cons.ext_name in
    let exception_name = exception_name_loc.txt in 
    let exception_kind = ext_cons.ext_kind in 

    let () = Printf.printf "%s" ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Exception Name "^exception_name) in 
    
    let id_exception_name = Var.fromString exception_name in 

    let tyd_exception = TyD.Ttop in 
    let refty_exception = RefTy.fromTyD tyd_exception in 


    match  exception_kind with
    | Text_decl (args, core_tyoption ) ->
              let () = Printf.printf "%s" ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Exception") in 
              let ex_refty_bind = (id_exception_name, toRefTyS refty_exception) in 
              let ve' = VE.add ve ex_refty_bind in 
              ve'
              
    | Text_rebind (_,_)-> (*Unimpl*) raise (ElebEnvFail "Rebinding Exception case not handled, only Exception decls allowed")







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
  let pat_type = vb_pat.pat_type in 
  (*see if pattern is [] or :: *)
  let ve' = match pat_desc with 
    
    | Tpat_construct (_, cons_desc, plist) ->
        let () = Printf.printf "%s" "&&\n" in 
        let consName = match cons_desc.cstr_name with 
                | "[]" -> "[]"
                | "::" -> "::"
                | _ -> cons_desc.cstr_name 
              in 
         if (consName = "[]" || consName = "::" || consName = "true" || consName = "false") 
          then ve 
        else       
        (let cosnArgs = cons_desc.cstr_args in 
        let resTy = cons_desc.cstr_res in 
        let destTyD = resTy in 
        let noramldestTyD = TyD.normalizeTypes destTyD in 
        let vid = Var.fromString (consName ) in 
        let conTyD = match cosnArgs with 
        | [] -> noramldestTyD 
        | argList -> 
          let tyDList = List.map (TyD.normalizeTypes) argList in 
          TyD.makeTarrow (TyD.Ttuple tyDList, noramldestTyD) in 


      let conTyS = toRefTyS (RefTy.fromTyD conTyD ) in 
      let () = Printf.printf "%s" ("ConTy "^(RefTyS.toString conTyS))
      in
      VE.add ve (vid,conTyS)
        )

(*      | Tpat_var (id,_) -> 
        let () = Printf.printf "%s" ("@@@@@@@@@1 "^Ident.name id) in 
        let patid = id in 
        let pat_type = pat_type in 
        let normal_pat_type = TyD.normalizeTypes pat_type in 
        (match normal_pat_type with 
          Tarrow (Ttuple [], Tbool ) -> ve 
          |_ ->
          (let () = Printf.printf "%s" ("TyD "^TyD.toString normal_pat_type) in 
           let patRefTy = RefTy.fromTyD normal_pat_type in
            let patTyS = toRefTyS patRefTy in 
            VE.add ve (patid, patTyS) ))
   
 *)     
    | _ ->
        
        let () = Printf.printf "%s" "VB other\n" in 
      ve  
    in 
    elabExpr ve' vb_expr 


and  elabCase ve  {c_lhs;c_rhs;_} =
 let pat_desc = c_lhs.pat_desc in 
  let pat_type = c_lhs.pat_type in 
  let vb_expr= c_rhs in 
  (*see if pattern is [] or :: *)
  let ve' =  match pat_desc with 
    | Tpat_construct (_, cons_desc, plist) ->
        let () = Printf.printf "%s" "&&\n" in 
        let consName =  match cons_desc.cstr_name with 
                | "[]" -> "[]"
                | "::" -> "::"
                | _ -> cons_desc.cstr_name 
              in 
        if(consName = "[]" || consName = "::" ||  consName = "true" || consName = "false")
          then ve 
        else       
        (let cosnArgs = cons_desc.cstr_args in 
        let resTy = cons_desc.cstr_res in 
        let destTyD = resTy in 
        let noramldestTyD = TyD.normalizeTypes destTyD in 
        let vid = Var.fromString (consName ) in 
        let conTyD = match cosnArgs with 
        | [] -> noramldestTyD 
        | argList -> 
          let tyDList = List.map (TyD.normalizeTypes) argList in 
          TyD.makeTarrow (TyD.Ttuple tyDList, noramldestTyD) in 
      let conRefTy =  RefTy.fromTyD conTyD in 
      let conTyS = toRefTyS conRefTy in 
      let () = Printf.printf "%s" ("ConTy "^(RefTyS.toString conTyS))
      
      in
      VE.add ve (vid,conTyS)
    )

(*      | Tpat_var (id,_) -> 
        let () = Printf.printf "%s" "@2" in 
        let patid = id in 
        let pat_type = pat_type in 
        let normal_pat_type = TyD.normalizeTypes pat_type in 
        let patRefTy = RefTy.fromTyD normal_pat_type in 
        let patRefTyS = toRefTyS patRefTy in 
        VE.add ve (patid, patRefTyS) 
   
 *)     |_ ->  
      let () = Printf.printf "%s" "case other\n" in 
       ve

      in 
      elabExpr ve' vb_expr 




open RefTy

let unifyConArgs (ve : VE.t) (con : Con.t) (vars : Var.t list) = 

    (* let () = Printf.printf "%s" ("@unifyConArgs") in 

  let () = List.iter (fun x -> Printf.printf "%s" ("Var "^Var.toString x^"\n") ) vars in *)
  let conStr = Con.toString con in 
                                                
  let convid = Var.fromString conStr in 
  let lenstr = fun v -> (string_of_int << List.length) v in 

  let conTy = try 
      RefTyS.specializeRefTy (VE.find ve convid)    
    with
    | _ -> raise (ElebEnvFail "Could not find constructor")
  in 
                                                
  match conTy with 
    Base _ -> let () =  assert(List.length vars = 0) in   
      []
  | Arrow ((argv,Base (argV,argTyD,_)),Base (_,datTyD,_)) -> 
        let () = assert (Vector.length vars = 1) in  
      List.map (fun (var) -> 
          (argV, var, argTyD, TyD.sametype argTyD datTyD)) vars

  | Arrow ((argv,Tuple tv), Base (_,datTyD,_)) ->
      let () = assert (Vector.length tv = Vector.length vars) in 
      
      let resUnified = 
      (List.map2 (fun (fldv, Base (argV,argTyD,_)) var ->
          ( argV, var, argTyD, 
           TyD.sametype argTyD datTyD)) tv vars) in 
      resUnified 
      
  (* | Arrow (_, Base (_,datTyD,_)) ->
     let () = Printf.printf "%s" ("@here10 ") in 
     let (paramsBind, fresTy) = RefTy.uncurry_Arrow conTy in 
     let paramsVars =
     let paramsTys = List.map (fun varTyBind -> (snd varTyBind)) paramsBind in 
     let resUnified = 
     (List.map2 (fun (fldv, Base (_,argTyD,_)) var ->
          (newLongVar (argv,fldv), var, argTyD, 
           TyD.sametype argTyD datTyD))  vars) in 
      let () = Printf.printf "%s" ("@here9 ") in 
      x 
   *)    
   
  |_ -> raise (ElebEnvFail "Could not unify and determine rec args")


let addRelToConTy (ve: VE.t) (con,letop,rexpr) (id:RelId.t) =
  let convid = Var.fromString (Con.toString con) in 

  let substs = match letop with 
      None ->
        []
    | Some lets -> 
       
        List.map (fun (cvar,var,_,_) -> (cvar,var))
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
  let ve = VE.remove ve convid in 
  VE.add (ve) (convid,newTyS)


  (*
   * Synthesizes the type of rexpr in the given relational env.
   * Rel Env is constructed during elaboration, hence this function
   * is also part of elaboration.
   *)


exception CantInferType
exception Return of TS.cs list * TS.t * RelLang.instexpr
exception Error of string 


let rec elabRExpr (re,pre,tyDB,spsB,rexpr) =

  let typeSynthRElem elem = match elem with
    (*What should be the type constructor for int and bool*)
      RelLang.Int i -> TyD.makeTint()
    | RelLang.Bool b -> TyD.makeTbool ()
    | RelLang.Var v -> 
        try 
          TyDBinds.find (tyDB) (v)  
        with 
        | (TyDBinds.KeyNotFound _) -> raise (Error ("Type inference failed "^(Var.toString v))) in 
  
  let () = Printf.printf "%s" "@here-elabRE-1  \n" in 
                                           
  let doIt (e1,e2) cons f = 
    let (cs1,tupTy1,e1') = elabRExpr (re,pre,tyDB,spsB,e1) in
    let () = Printf.printf "%s" "@here- doIt for second  \n" in 

    let (cs2,tupTy2,e2') = elabRExpr (re,pre,tyDB,spsB,e2) in 
    let (cs,tupTy) = f (tupTy1,tupTy2)
    in
    (mergecs [cs1;cs2;cs], tupTy, cons (e1',e2'))  in 

  let () = Printf.printf "%s" "@here-elabRE-2  \n" in 
            
  let isParam = fun rid -> SPSB.mem spsB rid in 
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
        let () = Printf.printf "%s" ("\n doItRInstApp 2") in   
      
       let open ParamRelEnv in 
       try 
       let _ = if isParam rel 
        then
         let (x,y,z) =  doItParamApp (rinst,tyd) in 
          raise (Return (x,y,z))
        else () in 
        let relName = RelId.toString rel in 

        let () = Printf.printf "%s" ("\n doItRInstApp "^relName) in   
        let {tys=relTyS;def} =           
          try (PRE.find pre rel) 
          with
          | PRE.ParamRelNotFound _ -> raise (ElebEnvFail ("Inst of unknown\
                                                          \ prim/param relation " ^relName)) in 
        let () = Printf.printf "%s" ("\n doItRInstApp definition foundd ") in   
        

        let _ = match def with 
            PRE.Prim _ -> 
              let ()= Printf.printf "%s" ("doItRInstApp-PrimCase\n") in   
              let (x,y,z) = doItPrimApp (rinst,tyd) in 
              raise (Return (x,y,z))
          (* Hack: For recursive applications. 
            Ashish : This is buggy for the cases where the recursive usage is not sufficient enough to have any sort information
          e.g.
            relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 

            *)

          | PRE.Bind Bind.BogusDef -> 
            let () = Printf.printf "%s" ("\n doItRInstApp BogusCase") in   
        
            (raise CantInferType )
          | _ -> () in 
        let tyd' = PTS.domain relTyS in 
        
        let () = Printf.printf "%s" ("doItRInstApp-2"^(TyD.toString tyd')) in   
        

        let targs = 
           match (tyd,tyd') with 
            (TyD.Tconstr (tycon1, targs), TyD.Tconstr (tycon2,_)) ->
                let () = Printf.printf "%s" ((TyD.toString tyd)^"\n") in 
                let () = Printf.printf "%s" ((TyD.toString tyd')^"\n") in 
     
                assert  (Tycon.equals (tycon1, tycon2))  ; targs
            | (_,TyD.Tvar _) -> [tyd]
            | _ -> raise (ElebEnvFail ("RelApp type mismatch: " ^ relName) ) in  

        

        
        let relSS = PTS.instantiate (relTyS,targs) in 

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
        with 
          | Return (a,b,c) -> (a,b,c)  
    in    

  let doItRInstApp = fun (rinst, x) ->
       let () = Printf.printf "%s" "doItRInstApp 1 \n" in 
   
        let tyd_found =  TyDB.find tyDB x in   
        let (cs,sort,rinst') = doItRInstApp (rinst, tyd_found)
        in
        let () = Printf.printf "%s" "doItRInstApp found\n" in 
   
        (cs,sort,R (rinst',x)) 

  in 
  (*function for the constructors as const can't be used as functions in OCAML*)
  let open RelLang in 
  let funU (e1, e2) = U (e1, e2) in 
  let funX (e1, e2) = X (e1, e2) in 
  let funD (e1, e2) = D (e1, e2) in 
  let funA (e1, e2) = ADD (e1 , e2) in 
  let funT td = TS.T td in
  match rexpr with
    U (v1, v2) -> 
          let () = Printf.printf "%s" ("elabRExpr Union ") in 
          doIt (v1,v2) (funU) TS.unionType 
  | X (v1,v2) -> 
        let () = Printf.printf "%s" ("elabRExpr X ") in 
        doIt (v1,v2) (funX) TS.crossPrdType
  | D (v1,v2) -> 
        let () = Printf.printf "%s" ("elabRExpr D ") in 
        doIt (v1,v2) (funD) TS.unionType 
  | ADD (v1, v2) ->
        (*The tuple sort for the Addition is similar to the union*)
        let () = Printf.printf "%s" ("elabRExpr ADD ") in 
        doIt (v1,v2) (funA) TS.unionType

  | T els -> 
        let () = Printf.printf "%s" ("elabRExpr T ") in 
          (emptycs(), TS.Tuple   
                (List.map (funT << typeSynthRElem) els) ,rexpr)
  | R (rinst,y) ->  
      let () = Printf.printf "%s" ("elabRExpr R ") in 
     
    doItRInstApp (rinst,y)  



let  elabPRBind (pre) (PR.T {id;def}) =
  (*This creates a new Tyvar without a name 'a , in the complete version this must be updated with noName*)
  let newVarTyD = fun _ -> TyD.makeTvar (Ident.create "int") in 
  let rec bindVars tyDB def =  
    match def with 
    | (PR.Nary (v,def)) -> bindVars (TyDB.add tyDB  v ( newVarTyD())) def  
    | (PR.Nullary rexpr) -> (tyDB, rexpr) 
  in   
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


let joinTyS tys1 tys2 = 
  let open TupSort in
  

  let PTS.T {sortscheme= ss1;_} = tys1 in  
  let PTS.T {sortscheme= ss2;_} = tys2 in  
  


  let PSS.T {sort = projSort1;_} = ss1 in 
  let PSS.T {sort = projSort2;_} = ss2 in 


  let PS.T {sort =sps1 ; _ } = projSort1 in 
  let PS.T {sort =sps2 ; _ } = projSort2 in 

  let tupsort1 = SPS.range sps1 in 
  let tupsort2 = SPS.range sps2 in 

 let tupleList1 = match tupsort1 with 
     TS.Tuple tTl -> tTl    
     | _ -> raise (ElebEnvFail "Unknow TupleSort")
 in 
 let tupleList2 = match tupsort2 with 
     TS.Tuple tTl -> tTl    
     | _ -> raise (ElebEnvFail "Unknow TupleSort")
  in    
  let joinedTys = if (List.length tupleList1 > List.length tupleList2) then tys1 else tys2 in 
  joinedTys



let elabSRBind (re)(pre)(ve ) (StructuralRelation.T {id;params;mapp} as sr) =

  let () = Printf.printf "%s" ("\n elabSRBind :: param size "^(string_of_int (List.length params))) in 
  let () = Printf.printf "%s" ("\n SR ::  "^(StructuralRelation.toString sr)) in 
  
  let open SPSBValue in 
  let open SPSBKey in   


  let spsB = List.fold_left (fun spsB r -> 
      SPSBinds.add spsB r {dom = ref None; range = SVar.newSVar ()}) SPSBinds.empty params in 
  
  let isParam = fun rid -> SPSB.mem spsB rid in 

  (* First pass - type & sort annotate instantiations *)

  let (map', relTySOp) = Vector.mapAndFold (mapp, None, 

                                            fun ((con,valop,rterm), relTySOp) ->
                                              let () = Printf.printf "%s" "\n Called " in 
                                              match (valop ,rterm) with
                                                (None,RelLang.Expr _) -> (* must be Rnull *)
                                                  let () = Printf.printf "%s" "\nCase 1" in 
                                                  ((con, valop  , rterm), relTySOp) 
                                              | (None,RelLang.Star ie) ->
                                                  let () = Printf.printf "%s" "\nCase Star" in 
                                              
                                                  let  _ = match relTySOp with
                                                      None -> ()
                                                    | Some _ -> raise (ElebEnvFail "Case of a wrong Star uasge") in 

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
                                                try   
                                                  let () = Printf.printf "%s" "\n Case elabSRBind Case Some Relation R" in 
                                                  
                                                  let convid = Var.fromString (Con.toString con) in 
                                                  let () = Printf.printf "%s" ("\n Convid "^Ident.name convid) in 
                                                  
                                                  let (RefTyS.T {tyvars;refss;_} as reftys) = 
                                                    try VE.find ve convid 
                                                    with 
                                                    | VE.VarNotFound _ -> 
                                                        (*Create the basic type for cosntructor*)
                                                        let s = "Constructor " ^(Con.toString con)^ " not found in var env." in 
                                                        raise (ElebEnvFail s) in 
                                                  
                                                  let () = Printf.printf "%s" ("\n RefTyS for convid  "^RefTyS.toString reftys ) in 
                                                  
                                                  let refty = RefSS.toRefTy refss in
                                                  let datTyD = match refty with
                                                      RefTy.Base (_,datTyD,_) -> datTyD
                                                    | RefTy.Arrow (_,RefTy.Base (_,datTyD,_)) -> datTyD
                                                    | _ -> raise (ElebEnvFail "Impossible case") in 
                                                  
                                                  let tyDB = List.fold_left (fun tyDB (_,var,tyD,_) -> TyDBinds.add tyDB var tyD)  
                                                      TyDBinds.empty (unifyConArgs ve con vars)in 

                                                       
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
                                                  let () = Printf.printf "%s" ("\n elabRExpr fails for "^(RelLang.exprToString rexpr) ) in                                          
                                                  let (cs,tupTy,rexpr') = elabRExpr (re, extendedPRE, (tyDB), spsB, rexpr) 
                                                   
                                                  in                                                  
                                                  let () = Printf.printf "%s" ("\n Tuple Type returned "^(TupSort.toString tupTy)) in 
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
                                                  let ptsIncoming = match relTySOp with
                                                      None -> bogusDesc.tys
                                                      | Some relTyS -> relTyS 
                                                  in       

                                                  let () = Printf.printf "%s" (" \n RelTySOP "^(ProjTypeScheme.toString ptsIncoming)) in 

                                                  let () = Printf.printf "%s" (" \n RelTyS "^(ProjTypeScheme.toString relTyS)) in 

                                                 

                                                    

                                                  let joined_relTyS = joinTyS ptsIncoming relTyS in 


                                                  let () = Printf.printf "%s" (" \n Joined RelTyS "^(ProjTypeScheme.toString joined_relTyS)) in 


                                                  ((con, valop  , RelLang.Expr rexpr'), Some joined_relTyS)

                                               with 
                                                | CantInferType -> 

                                                let () = Printf.printf "%s" ("returning Bogus Def ") in 
                                                ((con, valop, rterm),relTySOp)
                                               (*  match relTySOp with 
                                                   Some s -> ((con, valop, rterm),relTySOp)   
                                                  |None -> raise (ElebEnvFail ("\nThe relation "^(Ident.name id)^" does not have enough context to get a basic sort structure"))
 *)
                                              | _ -> 
                                                raise (ElebEnvFail "Impossible case of valop -rterm ") 
                                              ) in 


  let pts = match relTySOp with
      None -> 
        (*Hack to handle Rlen*)
        let conparam = TyD.Tvar (Tyvar.fromString "int") in 
        let list_cons = Tycon.fromString "list" in 
        let listconsparams = [conparam] in 
        let listTyD = TyD.makeTconstr (list_cons, listconsparams) in 
        PTS.simple ([],SPS.ColonArrow (listTyD,TS.Tuple [TS.T (conparam)]));
        
    | Some relTyS -> relTyS in 

  let () = Printf.printf "%s" (" \n RelTyS "^(ProjTypeScheme.toString pts)) in 

  let () = Printf.printf "%s" " \n pts :::::::::::::::::::::::" in 
  let () = Printf.printf "%s" (" \n "^RelId.toString id)  in 
    
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
   
    let () = Printf.printf "%s" (" \n merging funTyD from the OCaml compiler "^(TyD.toString tyd))  in 
    let () = Printf.printf "%s" (" \n merging funRefTy provided "^(RefTy.toString refTy))  in 



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

    | _ -> raise 

        (ElebEnvFail ("Types Merge Error. Cannot merge\n"
                               ^ "1. "^(L.toString $ RefTy.layout refTy)^", \n"
                               ^ "2. "^(TyD.toString tyd)^"\n") ) in 
  let (_,(_,refTy')) = 
    (* try 
     *)  doMerge tyd (genVar (), refTy)
    (* with 
    | _ -> (Vector.new0 (), (genVar(), refTy))   *)
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
        
        let () = Printf.printf "%s" (" \n Function artTy "^(TyD.toString normalargTyD))  in 
        let () = Printf.printf "%s" (" \n funRefTy remaining "^(TyD.toString normalbodyTyD))  in 


        let funTyD = TyD.makeTarrow(normalargTyD, normalbodyTyD) in 
        let fun_name =get_name_for_pat vb_pat in
        (*if the function type is being provided by the programmer use it, else create a simlple RefTY from the ocaml types*)
        let funTyS = 
          try 
            VE.find ve fun_name  
           with 
           | e ->  raise (e)

         in   

        let funRefTy = RefTyS.specializeRefTy 
                                     funTyS in  
        let () = Printf.printf "%s" (" \n funTyD from the OCaml compiler "^(TyD.toString funTyD))  in 
        let () = Printf.printf "%s" (" \n funRefTy provided "^(RefTy.toString funRefTy))  in 


        let funRefTy = mergeTypes (funTyD, funRefTy ) in 
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

        (*Shall we also add the type of each placeholder variable v_i in the environment*)

    | _-> ve in 
  List.fold_left (elab_vb) ve vb_list      


let elab_possible_fun_exp (ve, str_desc) = 

  let () =  Printf.printf "%s" "elaborating_possible_fun_exp" in 
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
  let veWithBool =  bootStrapBools VE.empty in 
  let veWithListCons = bootStrapCons veWithBool in 


  let  _ = Printf.printf "\n@Var Env Before:\n" in
  let  _ = Printf.printf "%s" ((VE.layout veWithBool)) in
  let  _ = Printf.printf "%s" "\n\n" in
 
  (*generate the types for the user-defined data types*)
  let initialVE = List.fold_left
      (fun ve str_item ->
         let str_item_desc = str_item.str_desc in 
         match str_item_desc with 
         | Tstr_value (flag, vbl) -> 
             List.fold_left (fun ve vb -> elabValueBind ve vb) ve vbl 
         | Tstr_eval (expr, attr) ->
             elabExpr ve expr
         | Tstr_type (rec_flag, type_decl_list) -> 
            let () = Printf.printf "%s" "\n elaborating Tstr_type" in 
            List.fold_left (fun veacc ty_decl -> elabDataTypeDecl veacc ty_decl) ve type_decl_list   
         | Tstr_exception(ext_cons) ->
            elabExceptionDecl ve ext_cons 

         |_ -> ve) veWithListCons tstr_items in 


   let  _ = Printf.printf "\n@Var Env After:\n" in
  let  _ = Printf.printf "%s" ((VE.layout initialVE)) in
    let  _ = Printf.printf "%s" "\n\n" in

  let  _ = Printf.printf "\n@PRE Initial Empty --------->:\n" in
  let  _ = Printf.printf "%s" (Layout.toString (PRE.layout PRE.empty)) in

  

  let  initialPRE = List.fold_left (fun (pre) (primbind) -> elabPRBind pre primbind) PRE.empty  primdecs in 

  let  _ = Printf.printf "\n@PRE after prim elab -----> :\n" in
  let  _ = Printf.printf "%s" (Layout.toString (PRE.layout initialPRE)) in

  
  
  let  initialRE =  RE.empty in 
  (*There is a common function to elaborate RE and PRE*)
  let  (elabRE,elabPRE) = List.fold_left (fun (re, pre) srbind -> 
     elabSRBind re pre initialVE srbind) (initialRE, initialPRE) reldecs  in 

  let  _ = Printf.printf "\n@RE Elab RE :\n" in
  let  _ = Printf.printf "%s" (Layout.toString (RE.layout elabRE)) in

  let  _ = Printf.printf "\n@PRE Elab PRE:\n" in
  let  _ = Printf.printf "%s" (Layout.toString (PRE.layout elabPRE)) in


  let  refinedVE = Vector.fold (RE.toVector elabRE, initialVE, 
                                fun ((id,{ty;map}),ve) -> Vector.fold (map, ve, 
           
                                                                       fun (conPatBind,ve) -> addRelToConTy ve conPatBind id)) in 
  (*The variable environment population using the user provided specifications*)
  let  protoVE = List.fold_left          
      (fun ve (TypeSpec.T {isAssume;name;params;refty}) -> 
         
         let () = Printf.printf "%s" (" ParamRefTy "^(RefTy.toString refty) ) in 
      (*    let rec correctedRefTy refty = match refty with 
          | Base (_,_,_) -> refty
          | Arrow ((argName, argTy), resTy) -> Arrow ( (Var.fromString "", argTy), correctedRefTy (resTy))
          | Tuple (_) -> refty
          in 
          let correctRefTy = correctedRefTy refty in 
         let () = Printf.printf "%s" (" ParamRefTyCorrected "^(RefTy.toString correctRefTy) ) in 
       *)    

         let  dummySPS = 
           SPS.ColonArrow ( TyD.makeTvar (Tyvar.fromString (""))
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

  let  _ = Printf.printf "\n@PROTO VE:\n" in
  let  _ = Printf.printf "%s" ((VE.layout protoVE)) in
  
  (*defined elab_structure_items in place of elabDecs for ocaml*)
  let  typedVE = elab_structure_items (protoVE, tstr_items) in 
   
  (*  let  _ = Printf.printf "\n@After Elab str:\n" in
    let  _ = Printf.printf "%s" ((VE.layout typedVE)) in

   *)let fullVE = List.concat [refinedVE;typedVE] in   

  (* let  fullVE = List.fold_left  
      (fun  (ve) (name,RefTyS.T {tyvars;isAssume;refss}) ->
         let  RefSS.T {prefty;_} = refss in 
         let  PRf.T {params=sortedParams;refty} = prefty in 
         let  (params,_) = Vector.unzip sortedParams in 
         let  refss' = elabTypeSpec (elabRE) (elabPRE) prefty in 
         let  refTyS = RefTyS.T {tyvars=tyvars;isAssume=isAssume; 
                                 refss=RefSS.fromRefTy refty} 
         in
         VE.add ve (name,refTyS)
      ) typedVE refinedVE
   *)

 (* 
   let  _ = Printf.printf "\n@FULL VE :\n" in
    let  _ = Printf.printf "%s" ((VE.layout fullVE)) in
 *)
  (fullVE,elabRE,elabPRE)
