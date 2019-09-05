open VerificationCondition
open SpecLang 
open Z3_encode

module TyD = TyD

  module RI = RelId
  module BP = Predicate.BasePredicate
  module RP = Predicate.RelPredicate
  module PR = PrimitiveRelation
  module L = Layout
  module VC = VerificationCondition
  type result = Success | Undef | Failure
 
  module FuncDecl = Z3.FuncDecl 
  module Solver = Z3.Solver 

  
  exception VCEncodingFailed of string 
  exception VCEex of string 

let ignore = fun _ -> ()

let z3_log = Z3_encode.logz3

 (* 
module Printf = struct 
  let printf f s = ()
  let originalPrint = Printf.printf 


end  
  *)



let discharge (VC.T ({tbinds=tydbinds;rbinds=pre}, anteP, conseqP)) =

	  let ctx = ref @@ Z3_encode.mkDefaultContext ()  in 
  let solver = ref @@ Solver.mk_solver !ctx None in 
  let () = Solver.reset !solver in   

     


  let constMap = ConstMap.empty in  
  let relMap = RelMap.empty in 

  (*Adding missing tydbinds*)
  let bnew_x = (Var.fromString "x", TyD.Tvar (Tyvar.fromString "int") ) in 
  let bnew_xs = (Var.fromString "xs", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "int")]) ) in 
  let bnew_l = (Var.fromString "l", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "int")]) ) in 
 
  let bnew_v_9 = (Var.fromString "v_9", TyD.Tvar (Tyvar.fromString "int") ) in 
  let bnew_el = (Var.fromString "el", TyD.Tvar (Tyvar.fromString "int") ) in 
  
  let bnew_v1 = (Var.fromString "v_1", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "int")]) ) in 
 let bnew_v13 = (Var.fromString "v_13", TyD.Tconstr(Tycon.fromString "btree", []) ) in 
 let bnew_lt = (Var.fromString "lt", TyD.Tconstr(Tycon.fromString "btree", []) ) in 

 let bnew_v11 = (Var.fromString "v_11", TyD.Tconstr(Tycon.fromString "btree", []) ) in 
  let bnew_rt = (Var.fromString "rt", TyD.Tconstr(Tycon.fromString "btree", []) ) in 

 let bnew_v_12 = (Var.fromString "v_12", TyD.Tvar (Tyvar.fromString "int") ) in 
 let bnew_n = (Var.fromString "n", TyD.Tvar (Tyvar.fromString "int") ) in 
  let bnew_v_1 = (Var.fromString "v_1", TyD.Tbool) in 
let bnew_v_0 = (Var.fromString "v_0", TyD.Tbool) in 
let bnew_v1 = (Var.fromString "v1", TyD.Tbool) in
let bnew_vp = (Var.fromString "vp", TyD.Tbool) in

 let bnew_v_10 = (Var.fromString "v_10", TyD.Tconstr(Tycon.fromString "pairList", []) ) in 
let bnew_temp4108 = (Var.fromString "temp4108", TyD.Tvar (Tyvar.fromString "int") ) in 
let bnew_temp4109 = (Var.fromString "temp4109", TyD.Tvar (Tyvar.fromString "int") ) in 
 let bnew_v_15 = (Var.fromString "v_15", TyD.Tconstr(Tycon.fromString "pairList", []) ) in 
  let bnew_b = (Var.fromString "b", TyD.Tbool ) in 
  

    let tydbinds = (*bnew_el :: bnew_lt :: bnew_rt:: bnew_n:: bnew_v_12:: bnew_v11:: bnew_v13::*)bnew_b:: bnew_v_15 :: (*bnew_temp4108 :: bnew_temp4109 ::bnew_vp:: bnew_v_16:: bnew_x::*)bnew_l:: (*bnew_v1:: bnew_v_0 :: bnew_v_1:: *)(*  bnew_v_9:: bnew_x :: bnew_xs::*) bnew_v_10:: tydbinds in 
 
  let pred0 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "n")), (Var (Var.fromString "v_12") )))) in 
  let pred1 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "lt")), (Var (Var.fromString "v_11") )))) in 
  
  let pred2 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "rt")), (Var (Var.fromString "v_13") )))) in 
  let pred3 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "el")), (Var (Var.fromString "v_9") )))) in 
  

  let anteP_extra_list = [(* pred0;pred1;pred2;pred3 *)] in
  let anteP_extra = Conj anteP_extra_list in 
  let anteP = Conj [anteP;anteP_extra] in 

  let newVC = VC.T ({tbinds=tydbinds;rbinds=pre}, anteP, conseqP) in 
  

  let sanitizeVC inVC  = 
      let VC.T ({tbinds=tydbinds;rbinds=pre}, anteP, conseqP) = inVC in 
      let visitedVar = [] in 
      let sanitizedList = List.fold_left 
                          (  fun visited (tx, ty) -> if (List.exists (fun (v,_) -> (Var.toString v = Var.toString tx)) visited) then visited else  ((tx,ty)::visited)) 
                          visitedVar tydbinds in 
        
      VC.T ({tbinds=sanitizedList;rbinds=pre}, anteP, conseqP)

  in     
  let sanitizedVC = sanitizeVC newVC in 
  let _ = Printf.printf "%s" ("Sanitized VCS") in 
  let _ = Printf.printf "%s" (L.toString (VC.layouts [sanitizedVC])) in 
  let _ = Printf.printf  "\n"  in 


  let VC.T ({tbinds=tydbinds;rbinds=pre}, anteP, conseqP) = sanitizedVC in 
      

  let tydbinds = tydbinds in 
  let rbinds = pre in 
  let anteP = anteP in 
  let conseqP = conseqP in 
    
         (*
       * Maps to keep track of encoded values
       *)
      
      (*int_sort and bool_sort are sort functions in MSFOL
        check how to encode MSFOL in Z3*)
       let tyMap = TyMap.empty  in 

      
      let tyMap = TyMap.add tyMap (TyD.Tint) (Int (Z3_encode.mk_int_sort ())) in 
      let tyMap = TyMap.add tyMap (TyD.Tbool) (Bool (Z3_encode.mk_bool_sort ())) in 
      let tyMap = TyMap.add tyMap (TyD.Tvar (Tyvar.fromString "int")) ( Int (Z3_encode.mk_int_sort ())) in 


      let addTyD tyMap tyd = 
                let sortToUninterpretedSort = 
                   fun sort -> 
                       (let tyMap = TyMap.add tyMap tyd sort in  
                            (tyMap, sort) )
                      in 
                  let tyName = Z3_encode.genTypeName () in        
                sortToUninterpretedSort (T (tyName, Z3_encode.mk_uninterpreted_s (tyName))) in 
      
      
       (*
       * bootStrapBools for constMap
       *)
      let constMap = ConstMap.empty  in 

      let constMap = ConstMap.add constMap "true" const_true in 
      let constMap = ConstMap.add constMap "false" const_false in 
      
        
      let getConstForVar constMap v = (
        fun vstr -> 
          try 
            ConstMap.find constMap vstr
          with 
            |Not_found  -> raise (ConstMap.ConstNotFound vstr)) (Var.toString v) in 
      
      let relMap = RelMap.empty in 

      let getStrucRelForRelId relMap rid = (fun ridstr ->  
          try
            RelMap.find relMap ridstr 
          with
          |Not_found  -> raise (RelMap.RelNotFound ("Rel "^ridstr^" undeclared despite processing tydbinds")) 
           
        ) 
          (RI.toString rid)
      in     
      
      let lookupRelId relMap rid = (
        fun rstr -> 
          try 
          RelMap.find  relMap rstr 
        with 
          | Not_found -> raise (RelMap.RelNotFound ("Rel "^rstr^" undeclared despite processing tydbinds"))
        
        ) (RI.toString rid)
      in 
    (*
       * Encoding functions
       * encodeConst and encodeStrucRel rely on uniqueness of 
       * bindings in tydbinds. In case of duplicate bindings,
       * duplication declarations show up in Z3 VC, but most
       * recent binding is used.
       *)

      let encodeTyD (tyMap, constMap, relMap) tyD  = 
        try 
         (tyMap, constMap, relMap, (TyMap.find tyMap tyD))  
        with 
        | TyMap.TyDNotFound _ -> 
          let () = Printf.printf "%s" ("@@@@@"^(TyD.toString tyD)) in 
      
          let sortfortyD = (match tyD with 
             TyD.Tvar _ ->  addTyD tyMap tyD 
            | TyD.Tconstr _ -> addTyD tyMap tyD
            | _ -> addTyD tyMap tyD  

            (* failwith "Unexpected type" *))

           in
           (*add to the tyMap*)
          let tyMap = TyMap.add tyMap tyD (snd (sortfortyD)) in 
         (tyMap, constMap, relMap, (snd (sortfortyD))) 
       in      

      let encodeConst (tyMap, constMap, relMap) (v,tyd)  = 
                
        let vstr = Var.toString v in 
        let () = Printf.printf "%s" ("\n encodeConst "^vstr^ "\n ") in 
             
        let (tyMap, constMap, relMap, sort) = encodeTyD (tyMap, constMap, relMap) tyd in 
      
        let  const = mkConst (vstr,sort) in 
        let constMap = ConstMap.add constMap vstr const
        in
          (tyMap, constMap, relMap, const)
         in 
        (**ERROR*)
        let encodeStrucRel (tyMap, constMap, relMap) (rid ,TyD.Tarrow (t1,_))  =
          let open TyD in 
          let  rstr = RI.toString rid in 
          let (tyMap, constMap, relMap,sorts) = 
            match t1 with 
              TyD.Ttuple tydr -> 
                let () = Printf.printf "%s" "\n encodeSTR case Tuple \n " in
                List.fold_left (fun (tyMap, constMap, relMap, sortsList) tyD -> 
                  let (tyMap, constMap, relMap, sort) = encodeTyD (tyMap, constMap, relMap) tyD in 
                  (tyMap, constMap, relMap, (sort::sortsList))   
                ) (tyMap, constMap, relMap, []) (tydr)  
              (*   List.map (fun tyD -> 
                                  let (tyMap, constMap, relMap, sort) = encodeTyD (tyMap, constMap, relMap) tyD in 
                                      sort) (tydr)  *)
              | _ ->  
                    let () = Printf.printf "%s" "\n encodeSTR case Other \n " in 
                    let (tyMap, constMap, relMap, sort) = encodeTyD (tyMap,constMap,relMap) t1 in  
                    (tyMap, constMap, relMap,Vector.new1 (sort))
          in 
          let () = Printf.printf "%s" "\n encodeSTR case sorts \n " in
                      
          let  sr = mkStrucRel (rstr, (List.rev sorts)) in

          let relMap = RelMap.add relMap rstr sr
        in
          (tyMap, constMap,relMap, sr)
       
      in 

  
       (* ---- Encoding TyD binds and relations ---- *)
      let open TyD in 
      let processTyDBind (tyMap, constMap, relMap) (v,tyd) = 
          
          let () = Printf.printf "%s" (" \nprocessTyDBind VCE "^(Var.toString v)) in 
          ( match tyd with  
        (*
         * Currently, the only values with function types
         * are structural relations encoded as functions from
         * a let or tuple of vals to bool.
         *)
          |Tarrow (t1,t2)   ->
                (match t2 with 
                  | Tbool ->                       
                        let (tyMap, constMap, relMap, sorts) = encodeStrucRel (tyMap, constMap, relMap) (RI.fromString (Var.toString v), tyd) in 
                        (tyMap, constMap, relMap)
                  | _ -> let (tyMap, constMap, relMap, _) = encodeConst (tyMap, constMap, relMap) (v,tyd) in 
                         (tyMap, constMap, relMap) 
                )         
          |_ -> 
              let () = Printf.printf "%s" (" \nprocessTyDBind consEncoding Case ") in 
          
              let (tyMap, constMap, relMap, _) = encodeConst (tyMap, constMap, relMap) (v,tyd) in 
            let () = Printf.printf "%s" (" \nprocessTyDBind consEncoding done ") in 
          
            (tyMap, constMap, relMap)
          ) 
           
    in 

    
    let processPrimEq (tyMap, constMap, relMap) (primR, def) =
         let () = Printf.printf "%s" (" \nprocessPrimEq PrimEq  ") in 
          
          (*
           * tbinds of VC.t are already processed. So Z3 relation
           * representing primR has been created already.
           *)
          let open Z3_encode in  
          let SR {ty;rel} = lookupRelId relMap primR in
          let sorts = ty in 
          let sr = rel in 
          (* 
           * primR is instantiated primitive relation. It has the
           * following form:
           *        primR = λv.rexpr
           * If primR is represented as a relation with sort T0*T1* ..
           * *Tn -> bool, then v has sort T0.
           *)

          let sort =
            try  
            List.nth sorts 0 
            with 
              | _ -> raise (VCEncodingFailed "empty sort list")  
          in 
          let (PR.Nary (v,PR.Nullary rexpr)) = def in 
          let vstr = Var.toString v in 
          let open RelLang in 
          let isBV = fun x -> Var.toString x = vstr in 
          let areBVs = fun els -> Vector.forall (els, 
              fun el ->
                match el with 
                Var x -> isBV x 
                | _ -> false) in 
          let areNotBVs = fun els -> Vector.forall (els, 
              fun el ->
                match el with 
                Var x -> not ( isBV x )
                | _ -> true) in 
          let len = Vector.length in 
          
          let encodeRelElem relel = 
            match relel with   
            |Int i -> mkInt i 
            | Bool true -> const_true 
            | Bool false -> const_false
            | Var v -> 
              try 
                getConstForVar constMap v
                with  
               | ConstMap.ConstNotFound v -> raise (VCEex ("Var "^v^" not found"))(* const_true  *)  
          in 

         
          let rec encodeQRelExpr  (e:expr) =
            match e with 
              T els -> 
                (match (len els, areBVs els, areNotBVs els) with
                (0,_,_) -> mkNullSet ()
              | (_,true,false) -> mkQSingletonSet  (Vector.map (els,
                  fun _ -> sort) )
              | (_,false,true) -> mkSingletonSet (Vector.map (els,
                  encodeRelElem))
              | _ -> raise (VCEncodingFailed "In primitive relation definition, each rexpr atom should either contain all bvs or none"))
            | X (e1,e2) -> mkCrossPrd (encodeQRelExpr e1, 
                encodeQRelExpr e2)
            | U (e1,e2) -> mkUnion (encodeQRelExpr e1, 
                encodeQRelExpr e2)
            | D (e1,e2) -> mkDiff (encodeQRelExpr e1, 
                encodeQRelExpr e2)
            | R (RInst {rel=rid; _},x) ->if isBV x 
              then mkQStrucRelApp (getStrucRelForRelId relMap rid)
              else mkStrucRelApp (getStrucRelForRelId relMap rid, 
                getConstForVar constMap v) in 
          let rhsSet = encodeQRelExpr rexpr in 
          let lhsSet = mkQStrucRelApp (SR {ty=ty;rel=rel}) in 
          let eqAssn = mkSetEqAssertion (lhsSet,rhsSet)
        in
         let () =  dischargeAssertion eqAssn
      
        in (tyMap, constMap, relMap)

      in   

   let  processBindEq (tyMap, constMap, relMap) (theR,def) : (TyMap.t* ConstMap.t* RelMap.t) =
          
          let () = Printf.printf "%s" (" Relation "^(RelId.toString theR) ) in 
          let Bind.Def {abs;_} = def in 
          let Bind.Abs (_,Bind.Expr {ground;fr}) = abs in 
          let (groundR, _,_) = ground in 
          let Bind.Fr (_,fre) = fr in 
          let open RelLang in 
          let rec doItFre fre = 
            match fre with 
             X (re1,re2) -> List.concat [doItFre re1; doItFre re2]
            |R (RInst {rel;_},_) -> [rel]
            | _ -> [theR](* raise (VCEncodingFailed "Transformer expression is not cross prd!") *)
          in   
          let paramRs = Vector.fromList (doItFre fre) in  
          
          let doItR = fun rid -> 
          mkQStrucRelApp( 
            getStrucRelForRelId relMap rid) in 
          let gSet = doItR groundR in 
          let pSets = Vector.map (paramRs, doItR) in 
          let frSet = mkQCrossPrd pSets in 
          let bindSet = mkBind (gSet,frSet) in 
          let  theSet = doItR theR 
        in
         let () =  assertBindEq (theSet,bindSet) in 
            (tyMap, constMap, relMap)
     
      in  
      
     (*  let _ = List.iter processTyDBind tydbinds in 
      *) (* pre is rbinds of elaborated VC.t. Maps newRelNames to
         instantiated definitions.*)
      let () = Printf.printf "%s" (" \n ******processTyDBind starting *****") in 
             
      let (tyMap, constMap, relMap) = List.fold_left (processTyDBind) (tyMap, constMap, relMap) tydbinds in  
      let () = Printf.printf "%s" (" \n ******** processTyDBind finished ********") in 
      
    
      let open PRE in 
     (*  let _ = List.iter 
        (fun (r,{def;_}) -> match def with
            PRE.Bind bdef -> processBindEq (r,bdef)
          | PRE.Prim pdef -> processPrimEq (r,pdef)) (PRE.toVector pre)

      *) 
      let (init_tyMap, init_constMap, init_relMap) = (tyMap, constMap, relMap) in    

    let () = Printf.printf "%s" (" \n ******PRE processing starting *****") in 
     
      let (tyMap, constMap, relMap) = 
        List.fold_left (fun (tyMap, constMap, relMap) (r,{def;_}) -> 
              match def with
              PRE.Bind bdef -> processBindEq (tyMap, constMap, relMap) (r,bdef)
            | PRE.Prim pdef -> processPrimEq (tyMap, constMap, relMap) (r,pdef)
          ) (init_tyMap, init_constMap, init_relMap) (PRE.toVector pre)

      in

  let () = Printf.printf "%s" (" \n ******PRE processing finished *****") in 
    
      (* pre is rbinds of elaborated VC.t. Maps newRelNames to
         instantiated definitions.*)
        
   (* ---- Type refinement encoding begins ---- *)

    let rec encodeBasePred (tyMap, constMap, relMap) (bp)  = 
        
      let () = Printf.printf "%s" (" \n ******encodeBasePred *****") in 
    
        let
          open BP in 
          let encodeBaseExpr bexp = 
            let () = Printf.printf "%s " "\n $$$$$$$$$$$$$$$$$-encdode BaseExp " in 
          
            match bexp with  
             (Int i) -> 
              let () = Printf.printf "%s " "\n $$$$$$$$$$$$$$$$$-encdode BaseExp Int " in 
          
              mkConst(string_of_int i, int_sort)
            | Bool true -> 
              let () = Printf.printf "%s " "\n $$$$$$$$$$$$$$$$$-encdode BaseExp BoolT " in 
          
                const_true 
            | Bool false -> 
              let () = Printf.printf "%s " "\n $$$$$$$$$$$$$$$$$-encdode BaseExp BoolF" in 
          
              const_false
            | Var v -> 
              let () = Printf.printf "%s " ("\n $$$$$$$$$$$$$$$$$-encdode BaseExp var "^(Ident.name v)) in 
          
            getConstForVar constMap v
        in
          let () = Printf.printf "%s " "\n $$$$$$$$$$$$$$$$$" in 
          match bp with
           Eq (e1,e2) -> 
            let () = Printf.printf "%s " "\n $$$$$$$$$$$$$$$$$-1 " in 
          
            mkConstEqAssertion 
              (encodeBaseExpr e1, encodeBaseExpr e2)
           | Iff (bp1,bp2) -> 
              let () = Printf.printf "%s " "\n $$$$$$$$$$$$$$$$$-2" in 
          
              mkIff ( (encodeBasePred (tyMap, constMap, relMap) bp1),
                (encodeBasePred (tyMap, constMap, relMap) bp2))
        in 
        
     let rec encodeRelExpr (tyMap, constMap, relMap) (e) =
    
      let () = Printf.printf "%s" (" \n ******encodeRelExpr *****") in 
    
        let open RelLang in 
        let encodeRelElem (tyMap, constMap, relMap) = fun x -> 
          match x with 
            | (Int i) -> mkInt i 
            | Bool true -> const_true 
            | Bool false -> const_false
            | Var v -> getConstForVar constMap v
        in
          let () = Printf.printf "%s" (" \n ******encodeRelExpr here *****") in 
    
         match e with 
            T els -> 
                let () = Printf.printf "%s" (" \n ******encodeRelExpr T *****") in 
      
                (
                match Vector.length els with 
                    0 -> mkNullSet ()
                    | _ -> mkSingletonSet(Vector.map (els,(encodeRelElem (tyMap, constMap, relMap)))))
            | X (e1,e2) -> 
              let () = Printf.printf "%s" (" \n ******encodeRelExpr X *****") in 
      
              mkCrossPrd ( (encodeRelExpr (tyMap, constMap, relMap) e1), 
                (encodeRelExpr (tyMap, constMap, relMap) e2))
            | U (e1,e2) -> 
              let () = Printf.printf "%s" (" \n ******encodeRelExpr U *****") in 
      
                mkUnion ( (encodeRelExpr (tyMap, constMap, relMap) e1), 
                (encodeRelExpr (tyMap, constMap, relMap) e2))
            | D (e1,e2) -> 
              let () = Printf.printf "%s" (" \n ******encodeRelExpr D *****") in 
      
              mkDiff ( (encodeRelExpr (tyMap, constMap, relMap) e1), 
                (encodeRelExpr (tyMap, constMap, relMap) e2))
            | R (RInst {rel=rid;_},v) -> 
              let () = Printf.printf "%s" (" \n ******encodeRelExpr R *****") in 
              
              (* try 
               *)mkStrucRelApp (
                (getStrucRelForRelId relMap rid), (getConstForVar constMap v))
              (*  with 
              | _ -> raise (VCEex "failed at mkStrucRel")
 *)
        in 

        
      let encodeRelPred (tyMap, constMap, relMap) (rp:RP.t)  =
                let () = Printf.printf "%s" (" \n ******encodeRelPred *****") in 

          let  open RelLang in 
        let f = (encodeRelExpr (tyMap, constMap, relMap)) in 
        let  open RP in 
        
        match  rp with 
          Eq (e1,e2) -> mkSetEqAssertion (f e1, f e2)
          | Sub (e1,e2) -> mkSubSetAssertion (f e1, f e2)
          | SubEq (e1,e2) -> (fun s -> mkOr( Vector.new2 (mkSetEqAssertion s,
              mkSubSetAssertion s))) (f e1, f e2)

        in 
        
       let encodeSimplePred (tyMap, constMap, relMap) (sp ) =
                let () = Printf.printf "%s" (" \n ******encodeSimplePred *****") in 

         match  sp with  
          (Base bp) -> encodeBasePred (tyMap, constMap, relMap) bp
        | (Rel rp) -> encodeRelPred (tyMap, constMap, relMap) rp in 

      let assertSimplePred  (tyMap, constMap, relMap) = dischargeAssertion << encodeSimplePred (tyMap, constMap, relMap) in 

      let rec encodeVCPred (tyMap, constMap, relMap) vcp = 

      let () = Printf.printf "%s" (" \n ******encodeVCPred *****") in 

      match vcp with 

          VC.Simple sp -> encodeSimplePred (tyMap, constMap, relMap) sp
        | VC.Conj vcps -> mkAnd (Vector.map (vcps, (encodeVCPred (tyMap, constMap, relMap)) ))
        | VC.Disj vcps -> mkOr (Vector.map (vcps, (encodeVCPred (tyMap, constMap, relMap)  )))
        | VC.Not vcp -> mkNot (encodeVCPred (tyMap, constMap, relMap) vcp)
        | VC.If (vcp1,vcp2) -> mkIf ((encodeVCPred (tyMap, constMap, relMap) vcp1), 
              (encodeVCPred (tyMap, constMap, relMap) vcp2))
        | VC.Iff (vcp1,vcp2) -> mkIff ((encodeVCPred (tyMap, constMap, relMap) vcp1), 
              (encodeVCPred (tyMap, constMap, relMap) vcp2)) in 

      let rec assertVCPred  (tyMap, constMap, relMap) vcp = 
       let () = Printf.printf "%s" (" \n ******assertVCPred *****") in 

      match vcp with 
          VC.Simple sp -> assertSimplePred (tyMap, constMap, relMap) sp
        | VC.Conj spv -> List.iter (assertVCPred (tyMap, constMap, relMap)) spv
        | _ -> dischargeAssertion (encodeVCPred (tyMap, constMap, relMap) vcp) in 

      let _ = assertVCPred (tyMap, constMap, relMap) anteP in 
      (*
       * We check the SAT of ¬conseqP
       *)
      let _ = dischargeAssertion (mkNot (encodeVCPred (tyMap, constMap, relMap) conseqP)) in 
      let solverDischarged = getSolver () in 
      let expressions_list = Solver.get_assertions solverDischarged  in  


      let () = Printf.printf "%s" ("\n# of Z3 expressions "^(string_of_int (List.length expressions_list))) in   
      let () = Printf.printf "%s" ("\nsolver \n "^(Solver.to_string solverDischarged)) in   
     
      let res =   Solver.check solverDischarged [] in
(*        let unsat_core = Solver.get_unsat_core solverDischarged in 
 
       let () = Printf.printf "%s" ("\nUnsat_core  ") in   
      let () = List.iter (fun exp -> Printf.printf  "%s" ("\n "^Z3_encode.Expr.to_string exp)) unsat_core in 
 *)       
 
      let () = Solver.reset solverDischarged in 
     
    
      match  res with 
           SATISFIABLE -> Failure 
         | UNKNOWN -> Undef 
         | UNSATISFIABLE -> Success
        | _ -> failwith "Integer received when Z3_lbool expected"


 
    