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

  
  exception TyDNotFound
  exception ConstNotFound
  exception RelNotFound of string
  exception VCEncodingFailed of string 

let ignore = fun _ -> ()

let z3_log = Z3_encode.logz3
 module TypeDMapKey =
       struct
         type t = TyD.t
         let equal(t1,t2)  =  TyD.sametype t1 t2
         let layout = Layout.str << TyD.toString 
       end

 module TypeDMapValue =
       struct
         type t = Z3_encode.sort
         let layout = Z3_encode.sort_layout
               end


module TyMap = SpecMap.ApplicativeMap (TypeDMapKey) (TypeDMapValue) 
let tyMap = TyMap.empty 

let strEq str1 str2 = (str1 = str2)  

module ConstantMapKey = 
       struct
         type t = string
         let equal (t1, t2)  =  strEq t1 t2
         let layout t = Layout.str t
       end

module ConstantMapValue = 
       struct
         type t = Z3_encode.ast
         let layout = Z3_encode.ast_layout
        end

module ConstMap = SpecMap.ApplicativeMap (ConstantMapKey) (ConstantMapValue)
let constMap = ConstMap.empty 

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
let relMap = RelMap.empty

let discharge (VC.T ({tbinds=tydbinds;rbinds=pre}, anteP, conseqP)) =

	 

    let ctx = ref @@ Z3_encode.mkDefaultContext ()  in 
    let solver = ref @@ Solver.mk_solver !ctx None in   

     
        (*
       * Maps to keep track of encoded values
       *)
       let tyMap = TyMap.empty in 
  
      let intTyD =  TyD.Tint   in 
      let boolTyD = TyD.Tbool   in 
      (*int_sort and bool_sort are sort functions in MSFOL
      	check how to encode MSFOL in Z3*)
      let _ = TyMap.add tyMap intTyD (Int (Z3_encode.mk_int_sort ())) in 
      let _ = TyMap.add tyMap boolTyD (Bool (Z3_encode.mk_bool_sort ())) in 
      let addTyD tyd = (fun sort -> 
          (TyMap.add tyMap tyd sort ; sort)) (T ("T", Z3_encode.mk_uninterpreted_s ("T"))) in 

      
       (*
       * bootStrapBools for constMap
       *)
      let constMap = ConstMap.empty  in 

      let _ = ConstMap.add constMap "true" const_true in 
      let _ = ConstMap.add constMap "false" const_false in 
      
        
      let getConstForVar v = (
      	fun vstr -> 
      		try 
      			ConstMap.find constMap vstr
        	with 
        		|Not_found  -> raise (ConstNotFound)) (Var.toString v) in 
      
      let relMap = RelMap.empty in 

      let getStrucRelForRelId rid = (fun ridstr ->  
      		try
      			RelMap.find relMap ridstr 
      		with
      		|Not_found  -> raise (RelNotFound ("Rel "^ridstr^" undeclared despite processing tydbinds")) 
      		 
      	) 
          (RI.toString rid)
      in     
      
      let lookupRelId rid = (
      	fun rstr -> 
        	try 
        	RelMap.find  relMap rstr 
        with 
        	| Not_found -> raise (RelNotFound ("Rel "^rstr^" undeclared despite processing tydbinds"))
        
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
        | TyMap.KeyNotFound _ -> 
           let () = Printf.printf "%s" ("@@@@@"^(TyD.toString tyD)) in 
   
        	(match tyD with 
        	 TyD.Tvar _ ->  addTyD tyD
            | TyD.Tconstr _ -> addTyD tyD
            | _ -> failwith "Unexpected type") in 

      let encodeConst (v,tyd) = 
        let vstr = Var.toString v in 
        let sort = encodeTyD tyd in 
           let () = Printf.printf "%s" "Location-------1>" in 
   
         let  const = mkConst (vstr,sort) in 
          let _ = ConstMap.add constMap vstr const
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
          let _ = RelMap.add relMap rstr sr
        in
          sr
       
      in 
       (* ---- Encoding TyD binds and relations ---- *)
       let open TyD in 
      let processTyDBind (v,tyd) = 
          let () = Printf.printf "%s" ((Var.toString v)^"\n") in 
          let () = Printf.printf "%s" (TyD.toString tyd) in 
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
          let sort = List.nth sorts 0 in 
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
            | Var v -> getConstForVar v 
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

      let _ = List.iter processTyDBind tydbinds in 
      (* pre is rbinds of elaborated VC.t. Maps newRelNames to
         instantiated definitions.*)
      let () = Printf.printf "%s" "Location-------1>" in 
   
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

        	

    