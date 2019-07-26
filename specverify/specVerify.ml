
open SpecLang
open Typedtree

module TyD = TyD
module PTS = ProjTypeScheme
module RefTy = RefinementType
module PRf = ParamRefType
module RefSS = RefinementSortScheme
module RefTyS = RefinementTypeScheme
module P = Predicate
module BP = Predicate.BasePredicate
module RP = Predicate.RelPredicate
module L = Layout
module VE = VarEnv
module VC = VerificationCondition
module PRE = ParamRelEnv 

module Tyvar =
struct
  open Tyvar
  let equal= fun (t,t') -> toString t = toString t'
  let eq = equal
end

type lambda = {var: pattern; body : expression}

type subst = Var.t*Var.t
type substs = subst list

let ($) (f,arg) = f arg
exception Destructpair of string
let fst = fun x ->
  match x with 
  | (a,b) -> a
  | _ -> raise (Destructpair " Trying to destruct a non-pair" )

let snd = fun x ->
  match x with 
  | (a,b) -> b
  | _ -> raise (Destructpair " Trying to destruct a non-pair" )

let empty = fun _ -> Vector.new0 ()

let unifiable = TyD.unifiable

let toRefTyS refTy = RefTyS.generalize (Vector.new0(), 
                                        RefSS.fromRefTy refTy)
let count = ref 4096

let getUniqueId symbase =
  let id = symbase ^ (string_of_int (!count)) in 
  let _ = count := !count + 1
  in
  Var.fromString id 

let genVar () =  getUniqueId "sv_" 
let getUniqueMarker () = getUniqueId "_mark_"

let dummyRefTyS () = RefTyS.generalize (Vector.new0(),
                                        RefSS.fromRefTy (RefTy.fromTyD (TyD.makeTunknown())))

let  newLongVar = fun (var,fld) -> Var.fromString ((Var.toString var)^"."^(Var.toString fld))

let  varEq (v1,v2) = ((Var.toString v1) = (Var.toString v2))

(*What is tyvec , what is the purpose of this *)
(* let varToExpVal = fun (var,tyvec) -> 
   Exp.Val.Atom (Exp.Val.Var (var,tyvec))
*)
let markVE ve = 
  let
    marker = getUniqueMarker ()
  in
  (marker,VE.add ve (marker,dummyRefTyS()))

exception PatternNotId of string 
let get_var_from_pattern vbpat = 
  let pat_desc = vbpat.pat_desc in 
  let patternVar = match pat_desc with
    | Tpat_var (id, loc) -> id 
    | _ -> raise (PatternNotId "Pattern not an Id") in 
  patternVar 

let is_function_Exp exp = 
    let expdesc = exp.exp_desc in 
    match expdesc with 
    | Texp_function (_,_,_) -> true
    | _ -> false


exception SpecVerifyExc of string


let print s  = () (* = Printf.printf "%s" s in () 
 *)
(*
   * Decomposes single tuple bind of form v ↦ {x0:T0,x1:T1} to
   * multiple binds : [v.x0 ↦ T0, v.x1 ↦ T1]
   *)
let decomposeTupleBind = RefTy.decomposeTupleBind



(*
   * For functions with dependent types, bound variables within argument
   * types are refered from refinements of result types. At application
   * sites, actual program vars are substituted for formal boundvars.
   * unifyArgs returns such substitutions.
   * unifyArgs also returns new type bindings for actual vars. This is for
   * the sake of constructor pattern matches, where the returned type binds
   * contain type bindings for matched pattern vars.
   *)
let rec unifyArgs ((argv ,argTy) as argBind,
               argExp ) =
  let exp_desc = argExp.exp_desc in 
  let open Typedtree in  
  let open Path in 
  match (argTy, exp_desc) with
  | (RefTy.Base _, Texp_ident(p,_,_)) -> 
      let Pident (v) = p in 
      (Vector.new1 (v,argTy), Vector.new1 (v,argv))
  | (RefTy.Tuple argBinds',Texp_tuple expl) -> 
          (*
           * Unifies v:{1:T0,2:T1} with (v0,v1)
           * Returns binds = [v0 ↦ T0, v1 ↦ T1],
           *        substs = [v0/v.1, v1/v.2]
           *)
      let
        (reftyss,substss) = 
          let zipped = List.map2 (fun argBind' exp -> 
              let (binds,substs') = unifyArgs (argBind', exp) in 
              let substs = Vector.map (substs', fun (n,o') -> (n, newLongVar (argv,o')))
              in
              (binds,substs)
              ) argBinds' expl 
        in 
          Vector.unzip zipped 

      in

      (List.concat reftyss, List.concat substss)

  (* |  (RefTy.Tuple argBinds', Texp_record ) => 
     let
      val (reftyss,substss)= (Vector.unzip o Vector.map)
      (argBinds', fn (argBind') =>
        let
          val (argv',_) = argBind'
          val argvStr' = Var.toString argv'
          val lblatomvec = Record.toVector atomrec
          val indx = Vector.index (lblatomvec, fn (lbl,_) =>
            Field.toString lbl = argvStr')
          val (_,atom) = case indx of 
              SOME i => Vector.sub (lblatomvec,i)
            | NONE => Error.bug ("Field " ^ (argvStr') ^ 
                " could not be found.")
          val (binds,substs') = unifyArgs (argBind',Atom atom)
            val substs = Vector.map (substs', 
              fn (n,o') => (n, newLongVar (argv,o')))
        in
          (binds,substs)
        end)
     in
      (Vector.concatV reftyss, Vector.concatV substss)
  *) 
  |(RefTy.Tuple argBinds', Texp_ident(p,a,b)) -> 

      (* Unifying v0:{x0:T0,x1:T1} with v1 would return
       * v1 ↦ {x0:T0,x1:T1} as the only new bind. However,
       * all references v0 elements should now refer to v1
       * elements. Therefore, substs = [v1.x0/v0.x0, v1.x1/v0.x1]
      *)
      let Pident (v) = p in   
      let binds = Vector.new1 (v,argTy) in 
      let substs = List.concat  
          (Vector.map (argBinds', 
                       fun (argBind') ->
                         let (argv',_) = argBind' in 
                         let newVar = newLongVar (v,argv') in 
                         let newIdent = Texp_ident((Pident newVar), a, b) in
                         let new_exp = {argExp with exp_desc = newIdent} in  
                         let (_,substs') = unifyArgs (argBind', new_exp ) in 
                         let  substs = Vector.map (substs', 
                                                   fun (n,o') -> (n, newLongVar (argv,o')))
                         in
                         substs
                      )
          ) 
      in
      (binds,substs)
  | (RefTy.Arrow _, Texp_ident(p,_,_)) -> 
      let Pident (v) = p in   

      (Vector.new1 (v,argTy), 
       Vector.new1 (v,argv))
  | _ -> raise (SpecVerifyExc "Invalid argTy-argExpVal pair encountered")




(*Takes two refinement types and returns a unified type*)
let  rec unifyWithDisj refTy1  refTy2  =
  let open RefTy in 
  match (refTy1, refTy2) with
    (Base (_, TyD.Tunknown, _),_) -> refTy2
  | (_,Base (_, TyD.Tunknown, _)) -> refTy1
  (*Disjunction of Predicates*)
  |(Base (bv1,td1,pred1),Base (bv2,td2,pred2)) ->
      let _ = assert (TyD.sametype td1 td2) in 
      let pred1' = Predicate.applySubst (bv2,bv1) pred1
      in
      Base (bv2,td2,Predicate.dot (pred1',pred2))
  | (Tuple t1,Tuple t2) -> 
      let tuFun = fun l -> Tuple l 
      in 
      (tuFun
        (List.map2  
          (fun (v1,r1) (v2,r2) -> 
           let _ = assert (varEq (v1,v2)) 
           in
           (v2,unifyWithDisj (r1) (r2))) t1 t2))

  | (Arrow _,Arrow _) -> raise (SpecVerifyExc "Unimpl : Case returning arrow")
  | _ -> raise (SpecVerifyExc "Case rules types not unifiable")       



 (*
   * Invariant : bv and td of ty is unchanged.
   *)
let  wellFormedType (marker, markedVE , ty) 
  =
  let velist = VE.toVector markedVE in 
  let indx = Vector.index velist (fun (v,_) -> varEq (v,marker)) in 
  let i = match indx with 
    | Some i -> i 
    |_ -> raise (SpecVerifyExc "Marker absent")
  in 
  let prefix = Vector.prefix velist i in 
  let exttylist = List.map 
      (fun (v,rtys) -> (v,
                        RefSS.toRefTy (RefTyS.specialize rtys))
      ) prefix in 
  let  flattened = List.concat 
      (List.map (fun (v,ty)-> 
          match ty with
            RefTy.Tuple _ -> decomposeTupleBind (v,ty)
          | _ -> Vector.new1 (v,ty)) exttylist) in 
  let (tyDB,pred1 ) = List.fold_left (
      fun (clos,pred') (exvar,exty) ->
        match exty with 
          RefTy.Base (bv,extd,pred) -> (TyDBinds.add clos exvar extd,
                                        Predicate.conj (pred',Predicate.applySubst (exvar,bv) pred))
        | RefTy.Tuple _ -> raise (SpecVerifyExc "Tuple flattening incorrect\n")
        | _ -> (clos,pred')

    ) (TyDBinds.empty, Predicate.truee())  flattened  in 
  RefTy.mapBaseTy ty (fun (v,td,pred) -> (v,td,
                                          Predicate.exists (tyDB,Predicate.conj(pred1,pred))))








let rec  type_synth_exp (ve, pre, exp) = 
  let open Typedtree  in 
  let exp_desc = exp.exp_desc in 
  let exp_type = exp.exp_type in 
  let normal_exp_type = TyD.normalizeTypes exp_type in 

  let trivialAns = fun _ -> ([], RefTy.fromTyD (normal_exp_type)) in 

  
  match exp_desc with 

  | Texp_apply (fexp, arg_label_exp_list) ->
 
      let (_,fty) = type_synth_exp (ve, pre, fexp) in 
      let ((farg,fargty) as fargBind, fresty) = 
        match fty with 
        | RefTy.Arrow (x,y) -> (x,y) 
        | _ -> 
        let exp_str = "Type of 271 not an arrow" in 
        raise (SpecVerifyExc exp_str) in 
      (*The a-normal  form will have just one argument*)   
      let valExp =  List.nth arg_label_exp_list 0 in
      let exp_from_valExp  = 
          match (snd valExp) with 
            | Some e -> e 
            | None -> raise (SpecVerifyExc "Expression missing")
          in    
      let (_,argTy) = type_synth_exp (ve,pre,exp_from_valExp) in 
            (*
             *  Γ ⊢ argTy <: fargty
             *)

      let vcs = VC.fromTypeCheck (ve,pre,argTy,fargty) in         
  
       (*
             * Then, determine type of this expression by substituion of
             * actuals for formals.
             *)
      let  (_,substs) = unifyArgs (fargBind,exp_from_valExp) in 
      let resTy = RefTy.applySubsts substs fresty
      in
      (vcs,resTy)


  | Texp_ident (p, l, vd) ->
          let () = print "typesyntt exp1" in 
    
 
      let ident_var = match p with 
          Pident id -> id
        | _ -> raise (SpecVerifyExc "Only Pident handled ")in 
      let idRefTyS = VE.find ve ident_var in
      let idRefTy = RefTyS.specializeRefTy (idRefTyS) in 

      ([], idRefTy)
  | Texp_constant c -> 
                let () = print "typesyntt exp3" in 
    
 
      trivialAns ()


  | Texp_let (rf, vbl, exp) ->
      (*T-let rule from the paper*)
          let () = print "typesyntt exp4" in 
    
 

      let (marker, markedVE) = markVE ve in 
      let (vcs1 , refTys_bind) = 
        type_synth_value_bindings (ve, pre, vbl) in 

      let value_type_list = match refTys_bind with 
        | RefTy.Tuple (var_binding_list) -> var_binding_list 
        | _ -> raise (SpecVerifyExc "Type of  not a tuple") in  

      let extendedVE = List.fold_left (
          (* fun (ve -> (pair var* refTY) -> ve)
          *)  
          fun veacc (var, refty) -> 
            let tsrefty  = toRefTyS refty in 
            let ve' = VE.add veacc (var, tsrefty) in 
            ve' 

        ) ve value_type_list in 

      let (vcs2, type_body_exp) = type_synth_exp (extendedVE, pre, exp) in 

      (List.concat [vcs1;vcs2], 
       wellFormedType (marker,extendedVE,type_body_exp))
      (*patlist is a list of pattern and expression, i.e. list of cases *)
  | Texp_match (testexp, case_list, explist, p) ->
      (*\Gamma *)
      let (marker, markedVE) = markVE ve in 
      (* let pat_exp_list =  
        let patterns = List.map (fun cs -> cs.c_lhs) patlist in 
        let exps = List.map (fun es -> es.c_rhs) explist in 
        Vector.zip  patterns exps in 
 *)
    
      let folding_function = fun  (vcs,wftypes) ({ c_lhs;c_rhs;_}) ->
        let pat = c_lhs in 
        let expr = c_rhs in 
        let  valbind = {vb_pat = pat; vb_expr=expr; vb_attributes = []; vb_loc = Location.none } in 
        let (marker,markedVE) = markVE ve in 
        (* 
                 * tyvars used, if any, for type instantiations inside
                 * test are bound at any of the enclosing valbinds. Therefore,
                 * passing empty for tyvars is sound.
                 *)
    
        let (vcs1,extendedVE) = doIt_value_bindings (markedVE,
                                                     pre, [valbind]) in 
        let (vcs2,ty) = type_synth_exp (extendedVE,pre,testexp) in 
        let wftype = wellFormedType (marker,extendedVE,ty)
        in
        (Vector.concat [vcs;vcs1;vcs2], wftype :: wftypes)
      in  
  
      let (vcs, wftypes ) = List.fold_left folding_function ([],[]) case_list in 
      let (wftypes, wftype) = Vector.split_last wftypes in

      let unifiedType = List.fold_left unifyWithDisj wftype wftypes in 
         (*
             * 1. alphaRename boundvars forall wfTypes to a single var
             * 2. assert that tyd is same for all
             * 3. fold all alpha-renamed preds of wftypes with Disj
     
            *)
     
      (vcs,unifiedType)


  (** try E with P1 -> E1 | ... | PN -> EN *)
  | Texp_tuple ( lsexp) ->

      let vcs_type_list = List.map (fun e -> type_synth_exp (ve, pre, e)) lsexp in 
      let (vcs_final, types_final) = List.fold_left (fun (accvcs, types) (vcs, ty) -> (List.concat [accvcs;vcs], ty::types))
          ([],[]) vcs_type_list in 
      (vcs_final, List.hd (List.rev types_final) )                   

  | Texp_construct (lc, cd, lexp) ->
      trivialAns ()

  (** C                []
      C E              [E]
      C (E1, ..., En)  [E1;...;En]

  *)
  | Texp_function (lab, csl, part) ->
      type_synth_function (ve, pre, exp_desc)

  | Texp_sequence (exp1 , exp2) -> type_synth_exp (ve,pre,exp2) 

  | _ -> trivialAns ()   



and type_synth_value_bindings (ve, pre, vblist) = 
    let open Typedtree in 
    let mapping_function  = 
      fun vb -> 
      let vbpattern  = vb.vb_pat in 
      let vbid = get_var_from_pattern vbpattern in 
      let vbexpression = vb.vb_expr in 
      (vbid, type_synth_exp (ve, pre, vbexpression))  
    in 
    (*[(vcs1, refty1);(vcs2, refty2);...]*)
    let list_pair_vcs_refty = List.map mapping_function vblist in 
    let folding_function = 
      fun (vcs, var_refty_binds) (vari, (vcsi,reftyi)) -> 
          (List.concat [vcs;vcsi], ((vari, reftyi)::var_refty_binds)) in 
    let (vcs, var_to_type_list) = List.fold_left folding_function ([], []) list_pair_vcs_refty in
    (vcs, RefTy.Tuple var_to_type_list) 
  


and type_check_function (ve, pre, fexp, ty)  =
  let ((_,argRefTy) as argBind, resRefTy) = 
    match ty with 
      RefTy.Arrow (x,y) -> (x,y ) 
    | _ -> raise (SpecVerifyExc "Function with non-arrow type")
  in 
  let () = print "typecheck fexp2" in 
     
  let (arg, fexp_body) = 
    (*assume single case*)
    let case_list = match fexp with 
        Texp_function(l, bd, _) -> bd in 

    let {c_lhs;c_guard;c_rhs} = List.nth  case_list 0 in
    ( c_lhs, c_rhs) 

  in 
  let argType = arg.pat_type.desc in 
  let extendedVE = VE.add ve ((get_var_from_pattern arg), toRefTyS argRefTy) in
  (*TODO correct this we do not have Exp.Val.Atom or Exp.Val.Var*)
  (* let  e_des = Texp_ident (path_n, Location.none
  let (binds, substs) = unifyArgs (argBind, (Exp.Val.Atom 
                                               (Exp.Val.Var (arg, Vector.new0 ())))) in 
  let _ = assert (List.length bind = 1) in 
  let resRefTy' = RefTy.applySubsts [] resRefTy in 
   *)     (*
       * Γ[arg↦argRefTy] ⊢ body <= resRefTy
       *)
  let () = print "typecheck fexp3" in 
      
  type_check_exp (extendedVE,pre,fexp_body,resRefTy)





and type_check_exp (ve, pre, exp, tyexp)  = 
  match exp.exp_desc with 
  | Texp_function (_,_,_) -> type_check_function(ve, pre, exp.exp_desc, tyexp)
  | _ -> 

        (*
         * Γ ⊢ exp => expRefTy
         *)
      let (expvcs,expRefTy) = type_synth_exp (ve,pre,exp) in 
        (*
         * Γ ⊢ expRefTy <: ty
         *)
      let () = print "Γ ⊢ expRefTy <: ty" in    
      let new_vcs = VC.fromTypeCheck (ve, pre, expRefTy, tyexp)  in 
      let () = print "generated VCS" in    
     
      List.concat [expvcs;new_vcs]


and doIt_lambda (ve, pre, lambda) =
  let var = lambda.var in 
  let body = lambda.body in

  let extendedPat = 
    match var.pat_desc with
    | Tpat_var (v,_)-> 
        (* let f = Tfun_exp exp*)
        let ftys = 
          try
            VE.find ve v  
          with 
          | _ -> raise (SpecVerifyExc "Impossible case")   
        in  
        let  RefSS.T {prefty = PRf.T {refty=fty;params};_} 
          = RefTyS.specialize ftys  in 
        let var_from_pat = get_var_from_pattern var in     
        let extendedVE = VE.add (VE.remove ve var_from_pat) (var_from_pat,
                                                    toRefTyS fty) in 

        let extendedPRE = Vector.fold (params, pre, 
                                       fun ((r,sps),pre) -> PRE.addUniterp pre (r, 
                                                                                PTS.simple (empty(),sps))) in 

        let vcs = match RefTyS.isAssumption ftys with
            false -> (Printf.printf "%s" ((Var.toString var_from_pat)^" will be\
                                                             \ checked\n"); 
                      type_check_function (extendedVE, extendedPRE, body.exp_desc , fty))
          | true -> Vector.new0 ()
        in 
           
        (vcs,  extendedVE)
  (*if not Tpat_var then unhandled*)
  | _ -> ([], ve)  
in 
extendedPat  

and type_synth_function (ve , pre , fexp) 
               =
  let (arg, fexp_body) = 
    (*assume single case*)
        let case_list = match fexp with 
             | Texp_function(l, bd, _) -> bd in 

        let {c_lhs;c_guard;c_rhs} = List.nth case_list 0 in
          ( c_lhs, c_rhs) 

  in 
  
  let argType = arg.pat_type in
  let normal_arg_type = TyD.normalizeTypes argType in  
  let argRefTy = RefTy.fromTyD (normal_arg_type) in 
  let argBind = (get_var_from_pattern arg,argRefTy) in 
      (*val _ = L.print (L.seq[Var.layout arg, L.str " :-> ",
        RefTy.layout argRefTy], print)*)
  let extendedVE = VE.add ve ((get_var_from_pattern arg), toRefTyS argRefTy) in
      (*
       * Γ[arg↦argTy] ⊢ body => bodyTy
       *)
  let (bodyvcs,bodyRefTy) = type_synth_exp (extendedVE, pre, fexp_body)
    in
  (bodyvcs,RefTy.Arrow (argBind,bodyRefTy))                 





(*Type checking value bindings, 
  create a lambda here and typecheck that *)
and doIt_value_bindings (ve, pre, valbinds) :(VC.t list * VE.t) = 
  let doIt_each_value_bind = 
    fun vb -> 
      let vbpat = vb.vb_pat in 
      let vbexp = vb.vb_expr in



      if (is_function_Exp vb.vb_expr) then 
        let () = print "doIt_value_bindings:: fexp" in 
        let lambda_for_vb = {var= vbpat; body = vb.vb_expr} in 
          doIt_lambda (ve, pre, lambda_for_vb)
      else
         let () = print "doIt_value_bindings:: fexp" in 
        
        doIt_exp (ve, pre, vbexp) 

  in 
  let vc_vepair_list = List.map doIt_each_value_bind valbinds in 
  let (vcs, tylist) = List.fold_left (
      fun (vcacc, ty_list) (vcs, vet) -> ((List.concat[vcacc;vcs]),(vet::ty_list))) ([], []) (vc_vepair_list) in 
  (*returning the type of last vb as the refTy *)
  (vcs, List.hd (List.rev tylist))






and  doIt_exp (ve, pre, exp) : (VC.t list * VE.t) = 
let () = print "doIt exp" in 
  let exp_desc = exp.exp_desc in 
  let exp_type = exp.exp_type in 

  (*do it find the function*)  
  match exp_desc with 
  | Texp_let (rf, vblist, exp) -> 
      let vcs_vb = doIt_value_bindings (ve, pre, vblist)in 
      vcs_vb
  | _ -> ([], ve)


let doIt_struct_items (ve, pre, tstr) = 
  let () = print "doIt sruct Items" in 

  (*Set the initial environment*)
  let struct_items = tstr.str_items  in 
  
  let doIt_struct_item  struct_item  = 
    match  struct_item.str_desc with 
    (*These are different Expressions which will generated an expression *) 
      Tstr_eval (exp , attr) ->  (*Any let expression at the top level  *)
        let () = print "Tstr_eval" in 

        let vcs_exp = doIt_exp (ve, pre, exp)
        in
        vcs_exp 

    | Tstr_value (rec_flag , value_bindings) -> (* and Tfun*)
        let () = print "Tstr_value" in 

        let vcs_vb = doIt_value_bindings (ve, pre, value_bindings) in 
        vcs_vb

    | _ -> ([], ve) in 
  
  let vcslist = 
    let listof_vcs_ve = 
      List.map (fun (st_item) -> doIt_struct_item (st_item)) struct_items in 
    let folding_function = 
      fun vcsacc (vcs, ves) -> (List.concat[vcsacc;vcs]) 
    in 
    List.fold_left (folding_function) [] listof_vcs_ve
  in 
  vcslist  


let doIt (ve, pre, tstr) = doIt_struct_items (ve, pre, tstr)
