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

let discharge pre= 


(*Verification Conditions Manual Creation*)
  let tdbinds = [] in
  (*131(x_1 :  'a_4258 list,
132          x_0 :  'a_4258 list,
133          nil :  'a_2 list,
134          anc_1024 :  'a_4258 list,
135          anc_1025 :  'a_4258 list,
136          l1 :  'a_4258 list,
137          l2 :  'a_4258 list)
*)


  let b1 = (Var.fromString "true", TyD.Tbool)  in
  let b2 = (Var.fromString "false", TyD.Tbool)  in
  let b3 = (Var.fromString "[]", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "'a")]) ) in 
let b4 = (Var.fromString "l1", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "'a")]) ) in 
let b5 = (Var.fromString "l2", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "'a")]) ) in 
let b6 = (Var.fromString "v_3", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "'a")]) ) in 
let b7 = (Var.fromString "xs", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "'a")]) ) in 
let b8 = (Var.fromString "x", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "'a")]) ) in 
let b9 = (Var.fromString "temp4099", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "'a")]) ) in 
let bnew_x = (Var.fromString "x", TyD.Tvar (Tyvar.fromString "'a") ) in 



(*   let b1 = (Var.fromString "x_1", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "a_4285")]) ) in 
let b2 = (Var.fromString "x_0", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "a_4285")]) ) in 
let b3 = (Var.fromString "nil", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "a_2")]) ) in
let b4 = (Var.fromString "anc_1024", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "a_4285")]) ) in 
let b5 = (Var.fromString "anc_1025", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "a_4285")]) ) in 
let b6 = (Var.fromString "l1", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "a_4285")]) ) in 
let b7 = (Var.fromString "l2", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "a_4285")]) ) in 
let b8 = (Var.fromString "v_6", TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "a_4285")]) ) in 
 *)
let tdbinds = [b1;b2;b3;b4;b5;b6;b7;b8;b9;bnew_x] in 
let rbinds = PRE.empty in 

let bindings = {tbinds = tdbinds; rbinds = rbinds}  in

(*
  Rhd  ::  {typescheme = : @ss: :.   _ list :-> {Tuple _}, def = \v_1. bind (Rhd : (v_1),\:v_0. {(v_0)})} 
   
 Rmem  ::  {typescheme = : @ss: :.   _ list :-> {Tuple _}, def = \v_3. bind (Rmem : (v_3),\:v_2. {(v_2)})} 
   
 Rob  ::  {typescheme = : @ss: :.   _ list :-> {Tuple __}, def = \v_6. bind (Rob : (v_6),\:v_4v_5. ({(v_4)} X {(v_5)}))} 
   
 Robs  ::  {typescheme = : @ss: :.   _ list :-> {Tuple __}, def = \v_9. bind (Robs : (v_9),\:v_7v_8. ({(v_7)} X {(v_8)}))} 

*)
let rhD_domain = (TyD.Tconstr(Tycon.fromString "list" , [TyD.Tvar (Tyvar.fromString "_")])) in
let rhD_range =  (TupSort.Tuple [TupSort.T (rhD_domain)]) in
let rhDSpS = SimpleProjSort.ColonArrow (rhD_domain, rhD_range) in 

let rob_domain = (TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "_")])) in
let rob_range =  (TupSort.Tuple [TupSort.T (rob_domain); TupSort.T (rob_domain)]) in
let robSpS = SimpleProjSort.ColonArrow (rob_domain, rob_range) in 


let robs_domain = (TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "_")])) in
let robs_range =  (TupSort.Tuple [TupSort.T (robs_domain); TupSort.T (robs_domain)]) in
let robsSpS = SimpleProjSort.ColonArrow (robs_domain, robs_range) in 



let rmem_domain = (TyD.Tconstr(Tycon.fromString "list",[TyD.Tvar (Tyvar.fromString "_")])) in
let rmem_range =  (TupSort.Tuple [TupSort.T (rmem_domain); TupSort.T (rmem_domain)]) in
let rmemSpS = SimpleProjSort.ColonArrow (rmem_domain, rmem_range) in 


let tydbindRhD = ((Var.fromString "Rhd"), rhDSpS) in 
let tydbindRob = ((Var.fromString "Rob"), robSpS) in 
let tydbindRobs = ((Var.fromString "Robs"), robsSpS) in 
let tydbindRmem = ((Var.fromString "Rmem"), rmemSpS) in 



let newsortbinds = [tydbindRhD;tydbindRob;tydbindRobs;tydbindRmem] in 


let newtydbinds = List.map 
                                (fun (rid, sort) ->
                                  let SPS.ColonArrow (tyd,TS.Tuple tts) = sort in 
                                  let tydvec =tyd :: (Vector.map (tts, 
                                                                  fun tts -> match  tts with
                                                                      TS.T tyd -> tyd 
                                                                    | TS.S t ->  raise (VCEncodingFailed "Unhandled Case") )) in 
                                  (*How we create a boolean type??*)
                                  let boolTyD = TyD.makeTbool () in 
                                  let relArgTyd = TyD.Ttuple (tydvec) in 
                                  let relTyD = TyD.makeTarrow ((relArgTyd), (boolTyD)) in
                                  let rtov = Var.fromString << RelId.toString in 
                                  let relvid = rtov rid
                                  in 
                                  (relvid,relTyD)
                               ) newsortbinds in 



let tdbinds = List.concat [tdbinds;newtydbinds] in 

(*iterate *)


let pre_vc_predList = [] in 


  (*
    1Robs(nil) = {()}
2       Rob(nil) = {()}
3       Rmem(nil) = {()}
4       Rhd(nil) = {()}
5       anc_1025 = x_1
6       anc_1024 = x_0
7       l2 = x_1
8       l2 = anc_1025
9       l1 = x_0
10       l1 = anc_1024
11       Robs(l1) = {()}
12       Rob(l1) = {()}
13       Rmem(l1) = {()}
14       Rhd(l1) = {()}
15       v_6 = x_1
16       v_6 = anc_1025
17       v_6 = l2

  *)
  let relIdRobs = RelId.fromString "Robs" in 
  let robsInst = RelLang.instOfRel relIdRobs in 
  
  let relIdRob = RelId.fromString "Rob" in 
  let robInst = RelLang.instOfRel relIdRob in 
  

  let relIdRhd = RelId.fromString "Rhd" in 
  let rhdInst = RelLang.instOfRel relIdRhd in 

  let relIdRmem = RelId.fromString "Rmem" in 
  let rmemInst = RelLang.instOfRel relIdRmem in 
  

  let pred1 =  Simple (Rel (RP.Eq (R (robsInst, Var.fromString "nil"), T []))) in 
  let pred2 = Simple (Rel (RP.Eq (R (robInst, Var.fromString "nil"), T []))) in 
  let pred3= Simple (Rel (RP.Eq (R (rmemInst, Var.fromString "nil"), T []))) in 
  let pred4 = Simple (Rel (RP.Eq (R (rhdInst, Var.fromString "nil"), T []))) in 

  let pred5 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "anc_1025")), Var ((Var.fromString "x_1") ))))  in 

  let pred6 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "anc_1024")), (Var (Var.fromString "x_0") ))))  in 

  let pred7 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "l2")), (Var (Var.fromString "x_1") )))) in 

  let pred8 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "l2")), (Var (Var.fromString "anc_1025") ))))  in 


  let pred9 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "l1")), (Var (Var.fromString "x_0") )))) in 
  let pred10 =Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "l1")), (Var (Var.fromString "anc_1024") ))))  in 






  let pred11 = Simple (Rel (RP.Eq (R (robsInst, Var.fromString "l1"), T []))) in 
  let pred12= Simple (Rel (RP.Eq (R (robInst, Var.fromString "l1"), T []))) in 
  let pred13= Simple (Rel (RP.Eq (R (rmemInst, Var.fromString "l1"), T []))) in 

  let pred14 = Simple (Rel (RP.Eq (R (rhdInst, Var.fromString "l1"), T []))) in 

  let pred15 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "v_6")), (Var (Var.fromString "x_1") ))))  in 

  let pred16 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "v_6")), (Var (Var.fromString "anc_1025") ))))  in 

  let pred17 = Simple (Base (BP.Eq ( 
                              (Var (Var.fromString "v_6")), (Var (Var.fromString "l2") ))))  in 


  let pre_vc_predList = [pred1;pred2;pred3;pred4;pred5;pred6;pred7;pred8;pred9;pred10;pred11;pred12;pred13;pred14;pred15;pred16;pred17] in 



  let pre_condition = Conj (pre_vc_predList) in 


  (*generate the post-condition*)
  (*Rmem(v_6) = (Rmem(l1) U Rmem(l2))
  *)


  let instVarRmem = Var.fromString "v_6" in 
  (*Rmem(v_6)*)

  let r_expr_lhs = RelLang.R (rmemInst, instVarRmem) in 
  let elhs = r_expr_lhs in 


  let instVarRmem_l1 = Var.fromString "l1" in
  let instVarRmem_l2 = Var.fromString "l2" in 
  (*Rmem(l1)*)
  let r_expr_l1 = RelLang.R (rmemInst, instVarRmem_l1) in 
  let r_expr_l2 = RelLang.R (rmemInst, instVarRmem_l2) in 
  
 


  let erhs = RelLang.U (r_expr_l1, r_expr_l2) in 
  let  post_condition = Simple (Rel (RP.Eq (elhs, erhs)))  in 
  let vcs = VC.T (bindings, pre_condition, post_condition) in 

  let _ = Printf.printf "%s" ("Verification Conditions \n") in 
  let _ = Printf.printf "%s" (L.toString (VC.layouts [vcs])) in 
     

  let anteP = pre_condition in 
  let conseqP = post_condition in 
  let tydbinds = tdbinds in 
  let pre = rbinds in 

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

      let addTyD tyMap tyd = (fun sort -> 
                                (let tyMap = TyMap.add tyMap tyd sort in  
                                      (tyMap, sort) )
                                                      ) 
                (T ("T", Z3_encode.mk_uninterpreted_s ("T"))) in 
      let () = Printf.printf "%s" ("TyMap size"^(string_of_int (List.length tyMap))) in 
   
      
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
         (tyMap, (TyMap.find tyMap tyD))  
        with 
        | TyMap.TyDNotFound _ -> 
           let () = Printf.printf "%s" ("@@@@@"^(TyD.toString tyD)) in 
   
          (match tyD with 
             TyD.Tvar _ ->  addTyD tyMap tyD 
            | TyD.Tconstr _ -> addTyD tyMap tyD
            | _ -> 

            failwith "Unexpected type") in 

      let encodeConst (tyMap, constMap, relMap) (v,tyd)  = 
        let vstr = Var.toString v in 
        let (tyMap, sort) = encodeTyD (tyMap, constMap, relMap) tyd in 
           let () = Printf.printf "%s" (" @@@@@@@@encoding const >"^vstr) in 
   
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
          let () = Printf.printf "%s" ("PTY ##########"^(Var.toString v)) in 
          

          ( match tyd with  
        (*
         * Currently, the only values with function types
         * are structural relations encoded as functions from
         * a let or tuple of vals to bool.
         *)
          |Tarrow (t1,t2)   ->
              let () = Printf.printf "%s" ("Case Tarrow  "^(Var.toString v)) in 
                (match t2 with 
                  | Tbool ->                       
                        let (relMap, sorts) = encodeStrucRel (tyMap, constMap, relMap) (RI.fromString (Var.toString v), tyd) in 
                        (tyMap, constMap, relMap)
                  | _ -> let (constMap, _) = encodeConst (tyMap, constMap, relMap) (v,tyd) in 
                         (tyMap, constMap, relMap) 
                )         
          |_ -> 
              let () = Printf.printf "%s" ("Case other"^(Var.toString v)) in 
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
      let () = Printf.printf "%s" "Location-------%%%%%%%%%%%>" in 
      let () = Printf.printf "%s" ("tydnidns size "^(string_of_int (List.length tydbinds))) in 
      
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
