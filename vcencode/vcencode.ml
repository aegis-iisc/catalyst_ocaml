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

let ignore = fun _ -> ()

let z3_log = Z3_encode.logz3



let discharge (VC.T ({tbinds=tydbinds;rbinds=pre}, anteP, conseqP)) =

	  let ctx = ref @@ Z3_encode.mkDefaultContext ()  in 
  let solver = ref @@ Solver.mk_solver !ctx None in   


  let constMap = ConstMap.empty in  
  let relMap = RelMap.empty in 

  (*Adding missing tydbinds*)
    let bnew = (Var.fromString "temp4099", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "'a")]) ) in 
  let bnew_x = (Var.fromString "x", TyD.Tvar (Tyvar.fromString "'a") ) in 
  let bnew_xs = (Var.fromString "xs", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "'a")]) ) in 
  
  (* let bnew_v5 = (Var.fromString "v_5", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "'a")]) ) in 
 (*  let bnew_v4 = (Var.fromString "v_4", TyD.Tvar (Tyvar.fromString "'a") ) in 
  *) 
   *)
   let tydbinds = bnew_xs::bnew_x:: bnew :: tydbinds in 
 
(*
  let relIdRobs = RelId.fromString "Robs" in 
  let robsInst = RelLang.instOfRel relIdRobs in 
  
  let relIdRob = RelId.fromString "Rob" in 
  let robInst = RelLang.instOfRel relIdRob in 
  

  let relIdRhd = RelId.fromString "Rhd" in 
  let rhdInst = RelLang.instOfRel relIdRhd in 

  let relIdRmem = RelId.fromString "Rmem" in 
  let rmemInst = RelLang.instOfRel relIdRmem in 
  
(* Rhd(l3) = {(x)}
  
 *)  
  let rhd_l3 =  Simple (
        Rel ( RP.Eq ( RelLang.R (rhdInst, Var.fromString "l3"), RelLang.T [ (RelLang.Var (Var.fromString ("x")))]))) in 
  let rmem_xs = RelLang.R (rmemInst, Var.fromString "xs") in 
  let tuple_x =  RelLang.T [(RelLang.Var (Var.fromString ("x")))] in 
  (*Rob(l3) = ({(x)} X Rmem(xs))
  *)
  let rob_l3 = Simple (Rel (RP.Eq ( RelLang.R (robInst, Var.fromString "l3"), 
                                    RelLang.X (tuple_x, rmem_xs))
                            )
                      ) in 

(*  Rmem(l3) = ({(x)} U (Rmem(xs) U {()}))
*)

  let rmem_l3= Simple (Rel (RP.Eq ( RelLang.R (rmemInst, Var.fromString "l3"), RelLang.U( tuple_x,  (RelLang.U (rmem_xs, RelLang.T []) )) ))) in 
  (*Robs(l3) = (({(x)} X Rmem(xs)) U (Robs(xs) U {()}))
*)
  let robs_xs = RelLang.R (robsInst, Var.fromString "xs") in 

  let robs_l3 = Simple (Rel (RP.Eq (RelLang.R (robsInst, Var.fromString "l3"), 
                                    (RelLang.U ( (RelLang.X( tuple_x, rmem_xs)),                 
                                                         
                                                (RelLang.U (robs_xs, RelLang.T []))
                                              )
                                    )
                                  ))) in 

  



   *) 

   (*  Rmem(temp4099) = ((Rmem(xs) U {(Rmem(l4))}))
*)
 
  let relIdRmem = RelId.fromString "Rmem" in 
  let rmemInst = RelLang.instOfRel relIdRmem in 
  
  let rmem_4099= Simple (Rel (RP.Eq ( RelLang.R (rmemInst, Var.fromString "temp4099"), RelLang.U((RelLang.R (rmemInst, Var.fromString "xs")),
                                                                                                (RelLang.R (rmemInst, Var.fromString "l4")) ) ) )) in 

   let pred_1= Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "temp4099")), Var ((Var.fromString "xs") ))))  in 
 (* 
let pred_2= Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "xs")), Var ((Var.fromString "l3") ))))  in 
 *)
(* 
let pred_3= Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "l4")), Var ((Var.fromString "v_7") ))))  in 
 *)
(* 
let pred_4 =  Simple (Rel (RP.Eq (RelLang.R (robsInst, Var.fromString "l3"), RelLang.T []))) in 
  let pred_5 = Simple (Rel (RP.Eq (RelLang.R (robInst, Var.fromString "l3"), RelLang.T []))) in 
  let pred_6= Simple (Rel (RP.Eq (RelLang.R (rmemInst, Var.fromString "l3"), RelLang.T []))) in 
  let pred_7 = Simple (Rel (RP.Eq (RelLang.R (rhdInst, Var.fromString "l3"), RelLang.T []))) in 
 *)

 
  (* let pred_2= Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "l1")), Var ((Var.fromString "xs") ))))  in 
   *)
  let pred_3= Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "temp4099")), Var ((Var.fromString "v_3") ))))  in 
 
  let anteP_extra_list = [(* rhd_l3;rob_l3; rmem_l3 ;robs_l3; *) rmem_4099(* pred_2; pred_3 pred_4;pred_5;pred_6;pred_7 *)] in


  let anteP_extra = Conj anteP_extra_list in 
  let anteP = Conj [anteP;anteP_extra] in 

   
  let newVC = VC.T ({tbinds=tydbinds;rbinds=pre}, anteP, conseqP) in 
  

  let _ = Printf.printf "%s" ("discharged VCS") in 
  let _ = Printf.printf "%s" (L.toString (VC.layouts [newVC])) in 
  let _ = Printf.printf  "\n"  in 
    
         (*
       * Maps to keep track of encoded values
       *)
      
      (*int_sort and bool_sort are sort functions in MSFOL
        check how to encode MSFOL in Z3*)
       let tyMap = TyMap.empty  in 

      
      let tyMap = TyMap.add tyMap (TyD.Tint) (Int (Z3_encode.mk_int_sort ())) in 
      let tyMap = TyMap.add tyMap (TyD.Tbool) (Bool (Z3_encode.mk_bool_sort ())) in 

      let addTyD tyMap tyd = (fun sort -> 
                                (let tyMap = TyMap.add tyMap tyd sort in  
                                      (tyMap, sort) )
                                                      ) 
                (T ("T", Z3_encode.mk_uninterpreted_s ("T"))) in 
      
      
       (*
       * bootStrapBools for constMap
       *)
      let constMap = ConstMap.empty  in 

      let constMap = ConstMap.add constMap "true" const_true in 
      let constMap = ConstMap.add constMap "false" const_false in 
      let constMap = ConstMap.add constMap "v_0" const_true in 
      
      let constMap = ConstMap.add constMap "v_1" const_false in 
      
        
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
         (tyMap, (TyMap.find tyMap tyD))  
        with 
        | TyMap.TyDNotFound _ -> 
       (*     let () = Printf.printf "%s" ("@@@@@"^(TyD.toString tyD)) in 
    *)
          (match tyD with 
             TyD.Tvar _ ->  addTyD tyMap tyD 
            | TyD.Tconstr _ -> addTyD tyMap tyD
            | _ -> 

            failwith "Unexpected type") in 

      let encodeConst (tyMap, constMap, relMap) (v,tyd)  = 
        let vstr = Var.toString v in 
        let (tyMap, sort) = encodeTyD (tyMap, constMap, relMap) tyd in 
      
         let  const = mkConst (vstr,sort) in 
        let constMap = ConstMap.add constMap vstr const
        in
          (constMap, const)
         in 
        
        let encodeStrucRel (tyMap, constMap, relMap) (rid ,TyD.Tarrow (t1,_))  =
          let open TyD in 
          let  rstr = RI.toString rid in 
            let sorts = match t1 with 
              TyD.Ttuple tydr -> List.map (fun tyD -> 
                                  let (tyMap, sort) = encodeTyD (tyMap, constMap, relMap) tyD in 
                                      sort) (tydr) 
            | _ ->  
                    let (_, sort) = encodeTyD (tyMap,constMap,relMap) t1 in  
                    Vector.new1 (sort) in 
          let  sr = mkStrucRel (rstr,sorts) in 
          let relMap = RelMap.add relMap rstr sr
        in
          (relMap, sr)
       
      in 

  (*   val _ = Vector.foreach (tydbinds, processTyDBind)
      (* pre is rbinds of elaborated VC.t. Maps newRelNames to
         instantiated definitions.*)
      val _ = Vector.foreach (PRE.toVector pre, 
        fn (r,{def,...}) => case def of
            PRE.Bind bdef => processBindEq (r,bdef)
          | PRE.Prim pdef => processPrimEq (r,pdef))
 *)

       (* ---- Encoding TyD binds and relations ---- *)
      let open TyD in 
      let processTyDBind (tyMap, constMap, relMap) (v,tyd) = 
          

          ( match tyd with  
        (*
         * Currently, the only values with function types
         * are structural relations encoded as functions from
         * a let or tuple of vals to bool.
         *)
          |Tarrow (t1,t2)   ->
                (match t2 with 
                  | Tbool ->                       
                        let (relMap, sorts) = encodeStrucRel (tyMap, constMap, relMap) (RI.fromString (Var.toString v), tyd) in 
                        (tyMap, constMap, relMap)
                  | _ -> let (constMap, _) = encodeConst (tyMap, constMap, relMap) (v,tyd) in 
                         (tyMap, constMap, relMap) 
                )         
          |_ -> 
              let (constMap, _) = encodeConst (tyMap, constMap, relMap) (v,tyd) in 
            (tyMap, constMap, relMap)
          ) 
           
    in 

    
    let processPrimEq (tyMap, constMap, relMap) (primR, def) =
       
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
               | ConstMap.ConstNotFound v -> const_true   
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
      let (tyMap, constMap, relMap) = List.fold_left (processTyDBind) (tyMap, constMap, relMap) tydbinds in  
      
    
      let open PRE in 
     (*  let _ = List.iter 
        (fun (r,{def;_}) -> match def with
            PRE.Bind bdef -> processBindEq (r,bdef)
          | PRE.Prim pdef -> processPrimEq (r,pdef)) (PRE.toVector pre)

      *) 
      let (init_tyMap, init_constMap, init_relMap) = (tyMap, constMap, relMap) in    

      let (tyMap, constMap, relMap) = 
      let () = Printf.printf "%s" "@reached here" in 
        List.fold_left (fun (tyMap, constMap, relMap) (r,{def;_}) -> 
            match def with
              PRE.Bind bdef -> processBindEq (tyMap, constMap, relMap) (r,bdef)
            | PRE.Prim pdef -> processPrimEq (tyMap, constMap, relMap) (r,pdef)
          ) (init_tyMap, init_constMap, init_relMap) (PRE.toVector pre)
  
      in

      (* pre is rbinds of elaborated VC.t. Maps newRelNames to
         instantiated definitions.*)
        
   (* ---- Type refinement encoding begins ---- *)

      let rec encodeBasePred (tyMap, constMap, relMap) (bp)  = 
        let
          open BP in 
          let encodeBaseExpr bexp = 
            match bexp with  
             (Int i) -> mkConst(string_of_int i, int_sort)
            | Bool true -> const_true 
            | Bool false -> const_false
            | Var v -> getConstForVar constMap v
        in
          match bp with
           Eq (e1,e2) -> mkConstEqAssertion 
              (encodeBaseExpr e1, encodeBaseExpr e2)
           | Iff (bp1,bp2) -> mkIff ( (encodeBasePred (tyMap, constMap, relMap) bp1),
                (encodeBasePred (tyMap, constMap, relMap) bp2))
        in 
        
     let rec encodeRelExpr (tyMap, constMap, relMap) (e) =
        let open RelLang in 
        let encodeRelElem (tyMap, constMap, relMap) = fun x -> 
          match x with 
            | (Int i) -> mkInt i 
            | Bool true -> const_true 
            | Bool false -> const_false
            | Var v -> getConstForVar constMap v
        in
         match e with 
            T els -> (
                match Vector.length els with 
                    0 -> mkNullSet ()
                    | _ -> mkSingletonSet(Vector.map (els,(encodeRelElem (tyMap, constMap, relMap)))))
            | X (e1,e2) -> mkCrossPrd ( (encodeRelExpr (tyMap, constMap, relMap) e1), 
                (encodeRelExpr (tyMap, constMap, relMap) e2))
            | U (e1,e2) -> mkUnion ( (encodeRelExpr (tyMap, constMap, relMap) e1), 
                (encodeRelExpr (tyMap, constMap, relMap) e2))
            | D (e1,e2) -> mkDiff ( (encodeRelExpr (tyMap, constMap, relMap) e1), 
                (encodeRelExpr (tyMap, constMap, relMap) e2))
            | R (RInst {rel=rid;_},v) -> mkStrucRelApp (
                (getStrucRelForRelId relMap rid), (getConstForVar constMap v))
        
        in 

      let encodeRelPred (tyMap, constMap, relMap) (rp:RP.t)  =
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
         match  sp with  
          (Base bp) -> encodeBasePred (tyMap, constMap, relMap) bp
        | (Rel rp) -> encodeRelPred (tyMap, constMap, relMap) rp in 

      let assertSimplePred  (tyMap, constMap, relMap) = dischargeAssertion << encodeSimplePred (tyMap, constMap, relMap) in 

      let rec encodeVCPred (tyMap, constMap, relMap) vcp = match vcp with 
          VC.Simple sp -> encodeSimplePred (tyMap, constMap, relMap) sp
        | VC.Conj vcps -> mkAnd (Vector.map (vcps, (encodeVCPred (tyMap, constMap, relMap)) ))
        | VC.Disj vcps -> mkOr (Vector.map (vcps, (encodeVCPred (tyMap, constMap, relMap)  )))
        | VC.Not vcp -> mkNot (encodeVCPred (tyMap, constMap, relMap) vcp)
        | VC.If (vcp1,vcp2) -> mkIf ((encodeVCPred (tyMap, constMap, relMap) vcp1), 
              (encodeVCPred (tyMap, constMap, relMap) vcp2))
        | VC.Iff (vcp1,vcp2) -> mkIff ((encodeVCPred (tyMap, constMap, relMap) vcp1), 
              (encodeVCPred (tyMap, constMap, relMap) vcp2)) in 

      let rec assertVCPred  (tyMap, constMap, relMap) vcp = match vcp with 
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

      let () = Printf.printf "%s" ("exp_list_size "^(string_of_int (List.length expressions_list))) in   
      let () = Printf.printf "%s" ("solver  "^(Solver.to_string solverDischarged)) in   
     
      let res =   Solver.check solverDischarged [] in 
     
    
      match  res with 
           SATISFIABLE -> Success 
         | UNKNOWN -> Undef 
         | UNSATISFIABLE -> Failure
        | _ -> failwith "Integer received when Z3_lbool expected"


 

(* 
   _______________________________

    let ctx = ref @@ Z3_encode.mkDefaultContext ()  in 
    let solver = ref @@ Solver.mk_solver !ctx None in   
   
    let constMap = ConstMap.empty in  
    let relMap = RelMap.empty in 
     
        (*
       * Maps to keep track of encoded values
       *)
      
      (*int_sort and bool_sort are sort functions in MSFOL
      	check how to encode MSFOL in Z3*)
       let tyMap = TyMap.empty  in 

       let () = Printf.printf "%s" ("TyMap size"^(string_of_int (List.length tyMap))) in 
   
      let tyMap = TyMap.add tyMap (TyD.Tint) (Int (Z3_encode.mk_int_sort ())) in 
      let tyMap = TyMap.add tyMap (TyD.Tbool) (Bool (Z3_encode.mk_bool_sort ())) in 
      let addTyD tyd = (fun sort -> 
          (TyMap.add tyMap tyd sort ; sort)) (T ("T", Z3_encode.mk_uninterpreted_s ("T"))) in 
      let () = Printf.printf "%s" ("TyMap size"^(string_of_int (List.length tyMap))) in 
   
      
       (*
       * bootStrapBools for constMap
       *)
      let constMap = ConstMap.empty  in 

      let constMap = ConstMap.add constMap "true" const_true in 
      let constMap = ConstMap.add constMap "false" const_false in 
      
        
      let getConstForVar v = (
      	fun vstr -> 
      		try 
      			ConstMap.find constMap vstr
        	with 
        		|Not_found  -> raise (ConstMap.ConstNotFound vstr)) (Var.toString v) in 
      
      let relMap = RelMap.empty in 

      let getStrucRelForRelId rid = (fun ridstr ->  
      		try
      			RelMap.find relMap ridstr 
      		with
      		|Not_found  -> raise (RelMap.RelNotFound ("Rel "^ridstr^" undeclared despite processing tydbinds")) 
      		 
      	) 
          (RI.toString rid)
      in     
      
      let lookupRelId rid = (
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
      let encodeTyD tyD = 
      	try 
         TyMap.find tyMap tyD  
        with 
        | TyMap.TyDNotFound _ -> 
           let () = Printf.printf "%s" ("@@@@@"^(TyD.toString tyD)) in 
   
        	(match tyD with 
        	   TyD.Tvar _ ->  addTyD tyD 
            | TyD.Tconstr _ -> addTyD tyD
            | _ -> 

            failwith "Unexpected type") in 

      let encodeConst (v,tyd) = 
        let vstr = Var.toString v in 
        let sort = encodeTyD tyd in 
           let () = Printf.printf "%s" (" @@@@@@@@encoding const >"^vstr) in 
   
         let  const = mkConst (vstr,sort) in 
          let constMap = ConstMap.add constMap vstr const
        in
          const
         in 
        
        let encodeStrucRel (rid ,TyD.Tarrow (t1,_)) =
          let open TyD in 
          let  rstr = RI.toString rid in 
          	let sorts = match t1 with 
              TyD.Ttuple tydr -> List.map encodeTyD (tydr)
            | _ -> Vector.new1 ( encodeTyD t1) in 
         	let  sr = mkStrucRel (rstr,sorts) in 
          let relMap = RelMap.add relMap rstr sr
        in
          sr
       
      in 
       (* ---- Encoding TyD binds and relations ---- *)
      let open TyD in 
      let processTyDBind (v,tyd) = 
          let () = Printf.printf "%s" ("PTY "^(Var.toString v)) in 
           
           match tyd with  
        (*
         * Currently, the only values with function types
         * are structural relations encoded as functions from
         * a let or tuple of vals to bool.
         *)
          |Tarrow (t1,t2)   ->
              let () = Printf.printf "%s" "Case Tarrow  " in 
                (match t2 with 
                  | Tbool -> ignore (encodeStrucRel (RI.fromString (Var.toString v), tyd) )
                  | _ -> ignore (encodeConst (v,tyd)))
          |_ -> 
              let () = Printf.printf "%s" "Case other" in 
              ignore(encodeConst (v,tyd))
           
    in 

    
    let processPrimEq (primR, def) =
       
          (*
           * tbinds of VC.t are already processed. So Z3 relation
           * representing primR has been created already.
           *)
          let open Z3_encode in  
          let SR {ty;rel} = lookupRelId primR in
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
                getConstForVar v
                with  
               | ConstMap.ConstNotFound v -> const_true   
          in 
          let rec encodeQRelExpr (e:expr) =
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
              then mkQStrucRelApp (getStrucRelForRelId rid)
              else mkStrucRelApp (getStrucRelForRelId rid, 
                getConstForVar v) in 
          let rhsSet = encodeQRelExpr rexpr in 
          let lhsSet = mkQStrucRelApp (SR {ty=ty;rel=rel}) in 
          let eqAssn = mkSetEqAssertion (lhsSet,rhsSet)
        in
          dischargeAssertion eqAssn
      
      	in 


	 let  processBindEq (theR,def) =
        
          let Bind.Def {abs;_} = def in 
          let Bind.Abs (_,Bind.Expr {ground;fr}) = abs in 
          let (groundR, _,_) = ground in 
          let Bind.Fr (_,fre) = fr in 
          let open RelLang in 
          let rec doItFre fre = 
            match fre with 
             X (re1,re2) -> List.concat [doItFre re1; doItFre re2]
            | R (RInst {rel;_},_) -> [rel]
            | _ -> raise (VCEncodingFailed "Transformer expression is not cross prd!")
          in   
          let paramRs = Vector.fromList (doItFre fre) in  
          let doItR = fun rid -> 
          mkQStrucRelApp( 
            getStrucRelForRelId rid) in 
          let gSet = doItR groundR in 
          let pSets = Vector.map (paramRs, doItR) in 
          let frSet = mkQCrossPrd pSets in 
          let bindSet = mkBind (gSet,frSet) in 
          let  theSet = doItR theR 
        in
          assertBindEq (theSet,bindSet)
               
       in 

      let () = Printf.printf "%s" "Location-------%%%%%%%%%%%>" in 
      let () = Printf.printf "%s" ("tydnidns size "^(string_of_int (List.length tydbinds))) in 
      
      let _ = List.iter processTyDBind tydbinds in 
      (* pre is rbinds of elaborated VC.t. Maps newRelNames to
         instantiated definitions.*)
   
     let () = Printf.printf "%s" "Location------->" in 

      let open PRE in 
      let _ = List.iter 
        (fun (r,{def;_}) -> match def with
            PRE.Bind bdef -> processBindEq (r,bdef)
          | PRE.Prim pdef -> processPrimEq (r,pdef)) (PRE.toVector pre)

  		in

      let () = Printf.printf "%s" "Location------->" in 

   (* ---- Type refinement encoding begins ---- *)

      let rec encodeBasePred (bp)  = 
        let
          open BP in 
          let encodeBaseExpr bexp = 
            match bexp with  
          	 (Int i) -> mkConst(string_of_int i, int_sort)
            | Bool true -> const_true 
            | Bool false -> const_false
            | Var v -> getConstForVar v
        in
          match bp with
           Eq (e1,e2) -> mkConstEqAssertion 
              (encodeBaseExpr e1, encodeBaseExpr e2)
           | Iff (bp1,bp2) -> mkIff (encodeBasePred bp1,
                encodeBasePred bp2)
        in 
        
	   let rec encodeRelExpr (e) =
        let open RelLang in 
        let encodeRelElem = fun x -> 
        	match x with 
        	| (Int i) -> mkInt i 
            | Bool true -> const_true 
            | Bool false -> const_false
            | Var v -> getConstForVar v
        in
         match e with 
          	T els -> (
          			match Vector.length els with 
                		0 -> mkNullSet ()
              			| _ -> mkSingletonSet(Vector.map (els,encodeRelElem)))
            | X (e1,e2) -> mkCrossPrd (encodeRelExpr e1, 
                encodeRelExpr e2)
            | U (e1,e2) -> mkUnion (encodeRelExpr e1, 
                encodeRelExpr e2)
            | D (e1,e2) -> mkDiff (encodeRelExpr e1, 
                encodeRelExpr e2)
            | R (RInst {rel=rid;_},v) -> mkStrucRelApp (
                getStrucRelForRelId rid, getConstForVar v)
      	
      	in 

      let encodeRelPred (rp:RP.t)  =
        let  open RelLang in 
        let f = encodeRelExpr in 
        let  open RP in 
        
        match  rp with 
        	Eq (e1,e2) -> mkSetEqAssertion (f e1, f e2)
          | Sub (e1,e2) -> mkSubSetAssertion (f e1, f e2)
          | SubEq (e1,e2) -> (fun s -> mkOr( Vector.new2 (mkSetEqAssertion s,
              mkSubSetAssertion s))) (f e1, f e2)

        in 
        
	     let encodeSimplePred (sp : VC.simple_pred) =
         match  sp with  
          (Base bp) -> encodeBasePred bp
        | (Rel rp) -> encodeRelPred rp in 

      let assertSimplePred  = dischargeAssertion << encodeSimplePred in 

      let rec encodeVCPred vcp = match vcp with 
          VC.Simple sp -> encodeSimplePred sp
        | VC.Conj vcps -> mkAnd (Vector.map (vcps, encodeVCPred))
        | VC.Disj vcps -> mkOr (Vector.map (vcps, encodeVCPred))
        | VC.Not vcp -> mkNot (encodeVCPred vcp)
        | VC.If (vcp1,vcp2) -> mkIf (encodeVCPred vcp1, 
              encodeVCPred vcp2)
        | VC.Iff (vcp1,vcp2) -> mkIff (encodeVCPred vcp1, 
              encodeVCPred vcp2) in 

      let rec assertVCPred   vcp = match vcp with 
          VC.Simple sp -> assertSimplePred sp
        | VC.Conj spv -> List.iter (assertVCPred) spv
        | _ -> dischargeAssertion (encodeVCPred vcp) in 

      let _ = assertVCPred anteP in 
      (*
       * We check the SAT of ¬conseqP
       *)
      let _ = dischargeAssertion (mkNot (encodeVCPred conseqP)) in 
      let expressions_list = Solver.get_assertions !solver  in  
      let res =   Solver.check !solver expressions_list in 
     
    
      match  res with 
      	   SATISFIABLE -> Success 
      	 | UNKNOWN -> Undef 
      	 | UNSATISFIABLE -> Failure
        | _ -> failwith "Integer received when Z3_lbool expected"
 *)
        	

    