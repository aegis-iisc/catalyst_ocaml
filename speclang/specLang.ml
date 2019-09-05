(* these need updation to OCAML Layout and TyDesc*)
module L = Layout (* define Layout *) 
module Ty = Types (* link to the Types*)


let debug = true 
let ($) (f, arg) =  f arg
let (<<) f g x = f (g x)
(* td :: define Control *)
let  localAssert = fun x -> true (* define  Control.assert*)
(* Check if the Var is the Var from the SpecLan , if from the SML, look for the front end in the OCAML*)
let varStrEq (v1, v2) = (Ident.name v1 = Ident.name v2)
let varSubs  (n, o) v = if (varStrEq (v, o)) 
  then n else v
let vecToStr f vec = match Vector.length vec with 
    0 -> "" 
  | _ -> " "^(Vector.toString f vec)

let empty = fun () -> Vector.new0 ()

(* See the use of this function and then define *) 
(*let mkMapper eqns cmp restore = fun t' ->
  	match ( List.peekMap 
       Define List.keepMap in Types_helper module
  	*) 
let mkMapper eqns cmp restore = 
  fun t' -> match (Vector.peekMap (eqns, (fun (t, ts) -> if t = t' then Some ts else None ))) with
      Some ts -> ts 
    |None -> restore t'

module Con = struct
  open Types
  open Asttypes
  type t = 
        Cons  
      | Nil 
      | True 
      | False 
      (* | Const of Asttypes.constant
       *)
      | NamedCons of Ident.t


  let toString t = match t with 
        Cons -> "::"
      | Nil -> "[]"
      | True -> "true"
      | False -> "false"
      | NamedCons id -> Ident.name id
      (* | Const t' ->  constant_to_string t'
 *)
  let default = Cons  
  let truee = True
  let falsee = False

  let fromString s = match s ="cons" with
    | true -> Cons 
    | false -> match s = "nil" with
        | true -> Nil
        | false -> NamedCons (Ident.create s)



end                    
(*Types varaiable 'a , 'b etc*)
module Tyvar = 
struct
  open Ident
  type t = Ident.t

  let symbase = "int"
  let  count = ref 0

  let newSVar () = Ident.create_persistent "int"


  (* fun _ ->
    let id = symbase ^ string_of_int !count in 
    let _ = count := !count + 1
    in 
    Ident.create_persistent id
 *)
  let equal (t1,t2) = Ident.equal t1 t2
  let toString = Ident.name
  let fromString s = create s

end 

module Var = struct
  open Ident
  
  let  count = ref 0
  let genVar = 
    fun _ ->
      let id = "_d"^(string_of_int !count) in 
    let _ = count := !count + 1
    in 
    Ident.create_persistent id

  type t = Ident.t
  let toString = Ident.name
  let fromString s = create s
  let layout t = Layout.str (toString t )
  let noName = create ""
  let equal t1 t2 = Ident.equal t1 t2
   



end

module RelId =
struct 
  open Ident
  type t = Ident.t
  let equal (t1,t2) = Ident.equal t1 t2
  let toString = Ident.name
  let fromString s = create s
end


module SVar = 
struct 
  type t = Ident.t

  let symbase = "'t"
  let  count = ref 0

  let newSVar = fun _ ->
    let id = symbase ^ string_of_int !count in 
    let _ = count := !count + 1
    in 
    Ident.create_persistent id

  let eq v1 v2 = Ident.equal v1 v2  
  let toString  = Ident.name  

end
(* define a module for the Type_desc and define helper functions on them
        * It will be easier to move this module to types module*)
exception SpecLangEx of string

exception TyConEx of string 

module Tycon= 
struct
  open Path
  type t = Path.t
  let toString  = Path.name  
  let equals(t1,t2) = (Path.name t1 = Path.name t2  )
  
  let is_ident  = Path.is_ident
  let fromString s = Path.mk_ident s 
end

     
module   TyD = 
struct
  open Types
  include Types
  
  (*Noramlize all other types into this type*)
  type t =
          Tunknown
        | Tvar of (Tyvar.t)
        | Tarrow of t * t
        | Ttuple of t list
        | Tconstr of Tycon.t * t list
        | Tfield of string * t
        | Tbool 
        | Tint
        | Ttop 
(*         | Tlink of t
 *)
  let makeTarrow (tdesc1, tdesc2) = Tarrow (tdesc1, tdesc2)
  (*given a list of tds [T1, T2, ... Tn ] and a destination type TD
    create Arrow 
  *)
  
  
  let makeTarrowExt (tdlist, tdest) =
    let td0 = List.nth tdlist 0 in
    let remainingList = Vector.sublist 1 ((List.length tdlist)-1) tdlist in  
    let prefix_arrow = List.fold_left (fun src dest -> makeTarrow(src, dest)) td0 remainingList

  in makeTarrow (prefix_arrow, tdest)

  let makeTconstr (cons, tdlist) = Tconstr (cons, tdlist)
  let makeTvar tvar = Tvar tvar
  let makeTfield (str, tdesc1) = Tfield (str, tdesc1)
  let makeTtuple tdlist = Ttuple tdlist
  let makeTunknown () = Tunknown
  let makeTbool () = Tbool 
  let makeTint () = Tint

    
  let rec visitType t = match t with 
        Tunknown -> "Tunknown"
      | Tvar v  -> "Tvar ("^(Tyvar.toString v)^")"
      | Tarrow (t1,t2) -> "Tarrow ("^(visitType t1)^","^(visitType t2)^")"
      | Ttuple tdl -> "Ttuple (["^(List.fold_left (fun s t ->(s^","^(visitType t))) "" tdl)^"])"
      | Tconstr (tc,tdl) -> "Tconstr("^(Tycon.toString tc)^","^"["^
                            (List.fold_left (fun s t -> (s^","^(visitType t))) "" tdl)^"])"
      | Tfield (s,td) -> "Tfield ("^s^","^(visitType td)^")"
      | Tbool -> "Tbool"
      | Tint -> "Tint"
      | Ttop -> "Ttop"
    

  let rec sametype t1 t2 = 
        (* let _ = 
          Printf.printf "%s" ("Asked if "^(visitType t1)^" and "^(visitType t2)^" are sametypes\n") in 
         *)  
      let rec sametypes tl1 tl2 = 
      (List.length tl1 = List.length tl2) &&  
          List.fold_left2 (fun flag t1 t2  -> (sametype t1 t2)) true tl1 tl2 
      in
      match (t1,t2) with 
         (Ttop, _) -> true 
         | (_, Ttop) -> true         
        |  (Tunknown,Tunknown) -> true
        | (Tvar v1, Tvar v2 ) -> Tyvar.equal(v1, v2)
        | (Tarrow (tda1,tdr1), Tarrow (tda2,tdr2)) -> 
            (sametype tda1 tda2) &&
            (sametype tdr1 tdr2)
        | (Ttuple td1, Ttuple td2) -> sametypes td1 td2
        | (Tconstr (tycon1,tdl1), Tconstr (tycon2,tdl2)) -> 
            let tyconeq = Tycon.equals (tycon1,tycon2)  in 
            (tyconeq)  && 
            (List.fold_left2 (fun acc x y -> acc && (sametype x y)) true tdl1 tdl2) 
        | (Tfield (s1,td1),Tfield (s2,td2)) -> (s1 = s2) && sametype td1 td2
        | (Tbool, Tbool) -> true
        | (Tint, Tint) -> true  
        | (_,_) -> false
      


    let rec toStrings (ts: t list) =
      match ts with                                         
         [] -> ""
       | [t] -> L.toString (L.seq [L.str " "; L.str (toString t)])
       | _ -> L.toString (L.seq [L.str " "; L.str (List.fold_left (fun acc l -> ((L.toString l)^acc)) ("") (Vector.map (ts, 
                                                                  L.str << toString))

      )])

    and toString t = 
      match t with
        Tunknown -> "<?>"
      | Tvar v  -> Tyvar.toString v
      | Tarrow (t1,t2)-> "("^(toString t1)^") -> ("^(toString t2)^")"
      | Ttuple tdrec -> "{" ^ (List.fold_left (fun acc td -> 
          (toString td)^acc^",") ("") (tdrec))^ "}"
      | Tconstr (tc,tdl) -> (toStrings tdl)^" "^(Tycon.toString tc)
      | Tbool -> "bool"
      | Tint -> "int"
      | Ttop -> "exception"

    

    let layout t = Layout.str (toString t)


        
  let instantiateTyvars substs tyd = 
      try 
      Tvar (List.assq tyd substs)
      with 
      | Not_found -> 
      let () = List.iter (fun (tyd1, tyvar) -> Printf.printf "%s" ((toString tyd1)^"|-> "^(Tyvar.toString tyvar))) substs in 
        raise (SpecLangEx ( "Type being Instantiated, Not present "^(toString tyd)))
   (*normalizeTypes : Types.type_expr -> TyD.t*)

      
  let rec normalizeTypes oc_type = 
      let oc_type_desc = oc_type.desc in 
      let oc_type_id = oc_type.id in 

      match oc_type_desc with 
        Types.Tvar s -> (match s with 
                          (*This is a hack currently we have only one type of Tyvar*)
                          Some s' -> Tvar (Tyvar.fromString s')
                          |None -> Tvar (Tyvar.fromString "int") 
                            (*no type variable means int type *)
                          )
       | Types.Tbool -> Tbool 

       | Types.Tint  -> Tint 
       | Types.Ttuple tel -> Ttuple (List.map (fun te -> normalizeTypes te) tel)
       | Types.Tconstr (p,tel,_) -> 
           if Tycon.is_ident p then
            let constPath = Tycon.toString p  in
            (*A hack to handle Ttyp constructor  arguments .e.g Node of int * ... will have int defined as a Tconst rather than Tvar in Ocaml*)
            match constPath with 
              | "int" -> Tvar (Tyvar.fromString "int") 
              | "bool"-> Tbool 
              | "exn" -> Ttop
              | _ -> Tconstr (p, (List.map (fun te -> normalizeTypes te) tel))
          else 
            raise (TyConEx "Only Identities allows as paths")  
       | Types.Tfield (s,_, te1,_) -> Tfield (s, normalizeTypes te1) 
       | Types.Tarrow (_,te1, te2 ,_) -> Tarrow (normalizeTypes te1, normalizeTypes te2)
             
        |  Types.Tobject (type_expr, _ ) -> (* Format.printf "found Tobject $$$$$$$$$$$$$$$$$$ \n";
           Format.printf "%a" Printtyp.type_expr (oc_type); *) Tunknown
      

        | Types.Tfield (_,_,_,_) -> Format.printf "found Tfield $$$$$$$$$$$$$$$$$$ \n";
           Format.printf "%a" Printtyp.type_expr (oc_type); Tunknown
      

        | Types.Tnil -> Format.printf "found Tnil $$$$$$$$$$$$$$$$$$  :: ";
           Format.printf "%a" Printtyp.type_expr (oc_type); Tunknown
      

        | Types.Tlink (te) -> 
           (*  Format.printf "found Tlink $$$$$$$$$$$$$$$$$$  :: ";
            Format.printf "%a" Printtyp.type_expr (oc_type);
            *) normalizeTypes te
          (* Tunknown *)
        | Types.Tsubst (te) -> Format.printf "found Tsubst $$$$$$$$$$$$$$$$$$ \n";
           Format.printf "%a" Printtyp.type_expr (oc_type); Tunknown
      

        | Types.Tvariant rd  -> Format.printf "found Tvariant $$$$$$$$$$$$$$$$$$ \n";
           Format.printf "%a" Printtyp.type_expr (oc_type); Tunknown
      

        | Types.Tunivar (str) -> Format.printf "found Tunivar $$$$$$$$$$$$$$$$$$ \n";
           Format.printf "%a" Printtyp.type_expr (oc_type); Tunknown
      

        | Types.Tpoly (te, tel) -> Format.printf "found Tpoly $$$$$$$$$$$$$$$$$$ \n";
           Format.printf "%a" Printtyp.type_expr (oc_type); Tunknown
      

        | Types.Tpackage (_,_,_) -> Format.printf "found Tpackage  $$$$$$$$$$$$$$$$$$ \n";
           Format.printf "%a" Printtyp.type_expr (oc_type); Tunknown
      

       | _ -> 
           Format.printf "found unknow $$$$$$$$$$$$$$$$$$ \n";
           Format.printf "%a" Printtyp.type_expr (oc_type);
      
              Tunknown

   
   (*In Ocaml all types are unifiable*)
  let unifiable (t1,t2) = 
    true
 (*  let makeTnil () = Types.Tnil 
 *)

end 

module TupSort = 
struct
  type tT = T of TyD.t | S of SVar.t
  type t = Tuple of tT list
  type cs = Eq of t*t

  let tTToString tT = match tT with 
      T tyd -> TyD.toString tyd
    | S svar -> SVar.toString svar


  let toString (Tuple tts) = List.fold_left (fun parsedStr tT ->  parsedStr^(tTToString tT)) "TupleSort " tts

  let toString = fun t -> "{"^(toString t)^"}" 


   let fromSVar = fun t ->  Tuple [S t]

  let filter_map  f l =
    let rec aux accu = function
      | [] -> List.rev accu
      | x :: l1 ->
          match f x with
          | None -> aux accu l1
          | Some v -> aux (v :: accu) l1
    in
    aux [] l 


  let getSVars (Tuple tts) = 
    filter_map (fun tT -> match tT with S t -> Some t | _ -> None) tts

  let tTeq t1 t2 = match (t1, t2) with 
    |(T tyd1, T tyd2) ->TyD.sametype tyd1 tyd2
    |(S v1, S v2) -> SVar.eq v1 v2
    | (_, _) -> false

  let equal (Tuple tydv1) (Tuple tydv2) =
    (List.length tydv1 = List.length tydv2) && 
    (List.for_all2 tTeq tydv1 tydv2)

  let unionType ((Tuple tydv1) as t1, (Tuple tydv2) as t2) = 
    match (List.length tydv1, List.length tydv2) with
    |(0, _) -> ([], Tuple tydv1)
    |(_, 0) -> ([], Tuple tydv2)
    |(n1, n2) -> if (equal (Tuple tydv1) (Tuple tydv2)) then ([], Tuple tydv1)
        else ([Eq (Tuple tydv1  ,Tuple tydv2)], Tuple tydv1)

  let crossPrdType (Tuple tyds1 as t1, Tuple tyds2 as t2) = 
    match (List.length tyds1, List.length tyds2) with
      (0,_) -> ([], Tuple tyds1)
    | (_,0) -> ([], Tuple tyds2)
    | _ -> ([], Tuple (List.concat [tyds1; tyds2]))

  let instSVars (Tuple ttl) mapSVar = 
    Tuple (List.concat  
          (List.map (fun tt -> match tt with 
                T _ -> [tt] 
                | S t -> (fun (Tuple ttl') -> ttl') (mapSVar t)) ttl) )  

  let mapTyD (Tuple ttl) f = Tuple (List.map (fun tt -> match tt with 
        T tyd -> T (f tyd)
      | _ -> tt) ttl )

  (* Tuple Sort Constraint Solving *)

  let eq = fun (Eq (x,y)) -> (x, y) 

  type sol = (SVar.t * t)
  
  let trySolveConstraint (c:cs) : sol option =
    let len = List.length in   
    
    let assertNotCirc (v, rt) = (* assert true in *) 
      let rhsvs = (getSVars rt) in 
      let cStr = fun _ -> (SVar.toString v)^" = "^ (toString rt) in
      let () = assert (List.for_all (fun rhsv ->
                       not (SVar.eq v rhsv)) rhsvs)  
        
      in () in   
     (match (eq c) with
      |(Tuple [S v1], Tuple []) -> None
      |(Tuple [], Tuple [S v2]) -> None
      |(Tuple [S v1], rty2) -> (assertNotCirc (v1, rty2); Some (v1, rty2))
      |(rty1, Tuple [S v2]) -> (assertNotCirc (v2, rty1); Some (v2, rty1))
      | _ -> None )
  
  let clearTautologies cs =
    let taut = fun (tt1, tt2) ->
      match (tt1, tt2) with
      |(T tyd1, T tyd2) -> (assert ( TyD.sametype tyd1 tyd2); true)
      |(S v1, S v2) -> SVar.eq v1 v2
      | _ -> false
    in 
    Types_helper.keepAll (fun (Eq (Tuple ts1, Tuple ts2)) -> 
        match (ts1, ts2) with
        | ([tt1], [tt2]) -> not (taut (tt1, tt2))
        | (_, _) ->  true) cs

  (* Applies reltyvar eqn( v=rt) iin cs.
   * Post condition : v does not occur in cs 
  *)
  let applyRelTyVarEqn eqn cs     =
    List.map (fun (Eq (rt1, rt2)) -> 
        let mapSVar = mkMapper [eqn] SVar.eq fromSVar in 
        let rt1' = instSVars rt1 mapSVar in 
        let rt2' = instSVars rt2 mapSVar in 
        Eq (rt1', rt2')) cs


  let elabcs cs  = 
    let len = List.length in 
    let (no, yes) = List.partition (fun (Eq (Tuple ts1, Tuple ts2)) ->  
        if (len ts1 = len ts2) then true else false) cs in
    let yes' = match yes with 
        [] -> failwith "Unsolvable TupSort Constraint"
      | Eq  (Tuple ts1, Tuple ts2) ::  yes' -> 
          List.concat [List.map2 (fun t1 t2 -> Eq (Tuple [t1], Tuple [t2] )) ts1 ts2; yes'] 
    in List.concat[yes';no]         

  (* Solve a constraint, applies the solution to the rest, and 
   * clears any new tautologoes. Repeats this process until threre are no more constraints, or the residue os 
   * unsolvable.
   * Invariant : 
           * sol :....
  *)
  exception Return of (sol list)

  let rec relTyVarEqns cs  =
    try
      let cs = clearTautologies cs in 
      let _ = match cs with
          [] -> (raise (Return []))
        | _ -> () 
      in (*This is a simplification, implement the correct semantics of Vector.loop this must return a pair*)
      let solEqnOp = Vector.loop(cs, (fun c -> match (trySolveConstraint c) with 
            Some sol -> Some (Some sol)
          | _ -> None), (fun () -> None) ) in

      (*The issue is that solEqnOp is returning a wrong type*)
      let (eqns, residue) = match solEqnOp with
          None -> ([], elabcs cs)
        | Some ((t, ts) as solEqn) -> 
            let newcs = applyRelTyVarEqn (t, ts) cs in 
            let moreEqns = relTyVarEqns newcs in 
            let moreEqnFn = mkMapper moreEqns SVar.eq fromSVar in 
            let solEqn' = match moreEqns with 
                [] -> solEqn
              |_ ->  (t, instSVars ts moreEqnFn)  in 
            (solEqn'::moreEqns, []) in 
      let newEqns = relTyVarEqns residue in                    
      let newEqFn = mkMapper newEqns SVar.eq fromSVar in 
      let eqns' = match newEqns with 
          [] -> eqns
        | _ -> List.map (fun (t, ts) -> (t, (instSVars ts newEqFn))) eqns in 

      List.concat [eqns'; newEqns]

    with
      Return l -> l

  (* Solve constraints and return solutions *)
  let solvecs cs = mkMapper (relTyVarEqns cs) SVar.eq fromSVar



end

module TS = TupSort
module SimpleProjSort = struct
  type t = ColonArrow of (TyD.t * TupSort.t)

  let toString (ColonArrow (tyD, tupsort)) = 
    (TyD.toString tyD)^ " :-> "^(TupSort.toString tupsort)
  (* The pretty-printing will be added later*)
  let layout t = Layout.str ( toString t)

  let newSPSort tyd rt  = ColonArrow (tyd, rt)

  let domain (ColonArrow (d, _)) = d

  let range (ColonArrow (_,r)) = r

  let mapTyD (ColonArrow (tyd, ts)) f =
    ColonArrow (
     tyd, TS.mapTyD ts f)
end

module SPS = SimpleProjSort 

module ProjSort = struct
  type t = T of {paramsorts : SimpleProjSort.t list;
                 sort : SimpleProjSort.t}

  let toString (T {paramsorts; sort}) =
    match List.length paramsorts with
      0 -> SimpleProjSort.toString sort
    |_ -> (Vector.toString (SimpleProjSort.toString) paramsorts)
          ^ " :-> "^ (SimpleProjSort.toString sort)

  let newProjSort paramsorts sort =  T {paramsorts = paramsorts; sort = sort}                

  let simple sps = T {paramsorts = Vector.new0 (); sort = sps}

  let domain (T {sort; _}) = SimpleProjSort.domain sort

  let range (T {sort; _}) = SimpleProjSort.range sort


end 

module PS = ProjSort 

module ProjSortScheme = struct 
  exception PSSInst of string 

  type t = T of {svars : SVar.t list; 
                 sort : ProjSort.t}

  let toString (T{svars;sort})= (Vector.toString (SVar.toString) svars)^". "^
                                (ProjSort.toString sort)

  let simple sps = T {svars = Vector.new0 (); sort = ProjSort.simple sps}


  let generalize (sv, s) = T {svars = sv; sort = s}

  let instantiate (T{svars;sort}, tsv) = 
    let svarMap = try 
        Vector.zip svars tsv 
      with 
      _ -> raise (PSSInst "PSS inst error1") in
    let mapsSVar = fun t -> 
      match (Vector.peekMap (svarMap, fun (t', ts) -> if (SVar.eq t t') then 
          Some ts else None ) )
      with 
        Some ts -> ts 
      |None -> raise (PSSInst "PSS inst error2") in 

    let PS.T {paramsorts;sort = (SPS.ColonArrow (tyd, ts)) } = sort in 
    let sort' = SPS.ColonArrow (tyd, TS.instSVars ts mapsSVar) in 
    let ps' = Vector.map (paramsorts, fun (SPS.ColonArrow (tyd,ts)) -> SPS.ColonArrow (tyd, TS.instSVars ts mapsSVar))  in 
    PS.T {paramsorts = ps'; sort = sort'}  

end

module PSS = ProjSortScheme

module ProjTypeScheme = struct 
  exception PTSInst of string
  type t = T of {tyvars : Tyvar.t list; sortscheme : ProjSortScheme.t}

  let toString (T {tyvars; sortscheme}) = (Vector.toString (Tyvar.toString) tyvars) 
                                          ^ " @ss: " ^ (ProjSortScheme.toString sortscheme)

  let paramSorts ( T {sortscheme = PSS.T {sort = ProjSort.T {paramsorts; _}; _}; _} ) = paramsorts

  let groundSort (T {sortscheme = PSS.T {sort = ProjSort.T{sort;_}; _};_}) = sort

  let simple (tyvars, sps) = T {tyvars = tyvars; sortscheme = PSS.simple sps}

  let generalize (tyvars, ss) = T {tyvars=tyvars; sortscheme = ss}
  (*Instantiate returns the same ProjSortScheme*)
  let instantiate (T {tyvars;sortscheme=ss} as pts, tydlist) = 
    let lentyDlist = (List.length tydlist) in
    let lentyvar = (List.length tyvars) in

    (* let tyvmap = try 
      (List.map2 (fun x y  -> (x,y)) tydlist tyvars) with 
        _ -> raise (PTSInst "PTS : insufficient or more type args") in
 *)
    let f = fun tyd -> tyd in  
     (* try    
      TyD.instantiateTyvars tyvmap 
    with 
    | _ -> raise (PTSInst "Error while instantiating Tyvars") 
  in 
 *)
    let PSS.T{sort = PS.T{paramsorts; sort = (SPS.ColonArrow (tyd, ts))}; svars} = ss in 
    
    let sort' = SPS.ColonArrow (f tyd, TS.mapTyD ts f) in 
    let ps' = List.map (fun (SPS.ColonArrow (tyd, ts)) -> SPS.ColonArrow (f tyd, ts)) paramsorts in 
    let s' = PS.T {paramsorts = ps'; sort = sort'} in 
    let ss' = PSS.T {svars = svars; sort = s'} in 

    ss'
  let domain (T {sortscheme = PSS.T { sort = ProjSort.T{sort = SPS.ColonArrow (tyd,_);_}; _};_ }) = tyd





end 
module PTS = ProjTypeScheme 
module RelLang = 
struct 
  type elem = Int of int 
            | Bool of bool
            | Var of Ident.t

  type instexpr = RInst of {targs: TyD.t list;
                            sargs : TupSort.t list;
                            args : instexpr list;
                            rel : RelId.t       }

  type expr = T of elem list 
            | X of expr * expr 
            | U of expr * expr 
            | D of expr * expr 
            | R of instexpr * Ident.t

  type term = Expr of expr
            |Star of instexpr

  let rId = fun c -> T  (Vector.new1 (Var c)) (* define Vector.new0 similar to the mlton basic type of vector*)

  let rNull = fun _ -> T ( empty ())

  let instOfRel = fun rel ->
    let empty = fun _ -> Vector.new0 () in 
    RInst {targs= empty (); sargs = empty (); args = empty (); rel= rel}

  let instOfPolyRel = fun rel -> 
    (fun targs -> let empty = fun _ -> Vector.new0 () in 
      RInst {targs=targs; sargs=empty (); args= empty (); rel= rel} )

  let appR = fun (rid, targs, x) -> R (instOfPolyRel rid targs, x)

  let elemToString = fun el -> 
    match el with 
      Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Var v -> v.name (* define the Var moudle*)

  let rec ieToString (RInst {targs; sargs; args; rel}) = 
    let tstr = vecToStr TyD.toString targs in 
    let sstr = vecToStr TupSort.toString sargs in 
    let rstr = vecToStr ieToString args in 
    "(" ^ (RelId.toString rel)^tstr^sstr^rstr ^ ")"

  let rec exprToString exp = match exp with 
      T elevc -> "{(" ^ (List.fold_left (fun acc e -> 
        (elemToString e)^acc) "" elevc) ^ ")}"
    | X (e1, e2) -> "(" ^ (exprToString e1) ^ " X " ^ (exprToString e2) ^ ")"         

    | U (e1, e2) -> "(" ^ (exprToString e1) ^ " U " ^ (exprToString e2) ^ ")"  

    | D (e1, e2) -> "(" ^ (exprToString e1) ^ " - " ^ (exprToString e2) ^ ")"      


    | R (ie, arg) ->  
        (ieToString ie) ^ "(" ^ (arg.name) ^ ")"

  let exprToString = exprToString


  let termToString = fun trm -> match trm with 
      Expr e -> exprToString e
    | Star ie -> (ieToString ie) ^ "*"

  (*fun app (relId,var) = R(relId,var)*)
  let union (e1, e2) = U (e1,e2)
  let crossprd (e1,e2) = X (e1,e2)
  let diff (e1,e2) = D (e1,e2)
  let rNull _ = T []


  let (%) = fun f g x  -> f (g x)  

  let rec ieApplySubsts substs (RInst {rel;args;targs;sargs}) = 
    let doIt = ieApplySubsts substs in 
    let vtor = ((RelId.fromString) % (SVar.toString)) in 
    let subst v = List.fold_left (fun r (n,o) -> if ( Ident.name o = RelId.toString r) then 
                                     (vtor n) else r) v substs in 
    RInst {rel=subst rel; args=List.map doIt args;targs=targs; sargs=sargs } 

  let rec applySubsts substs rexpr = 
    let doIt = applySubsts substs  in 
    let subst v = List.fold_left (fun v (newEl, oldEl) -> 
         if (Ident.name oldEl = Ident.name v) then newEl else v) v substs in 
    let elemSubst elem = match elem with 
        Var v -> 
          Var (subst v)
      | c -> c in
    match rexpr with 
      T elem -> T (List.map elemSubst elem)
    | X (e1, e2) -> X (doIt e1, doIt e2)
    | U (e1, e2) -> U (doIt e1, doIt e2)
    | D (e1, e2) -> D (doIt e1, doIt e2)
    | R (ie, argvar) -> R (ieApplySubsts substs ie, subst argvar)

  let rec mapInstExpr t f = 
    let g = fun x -> mapInstExpr x f in 
    let doIt = fun cons -> fun (x1, x2) -> cons (g x1 , g x2) in  

    match t with 
      X (x, y) ->  X ( g x, g y) 
    | U (x, y) -> U (g x, g y)
    | D (x, y) -> D (g x, g y)
    | T _ -> t
    | R (ie, x) -> R (f ie, x) 


  let mapTyD t f = 
    let rec doItIE (RInst{targs;sargs;args;rel}) = 
      let targs' = List.map f targs in
      let sargs' = List.map (fun ts -> TS.mapTyD ts f) sargs in 
      let args' =  List.map doItIE args in 
      RInst {targs=targs'; sargs=sargs'; args=args';rel=rel}

    in 
    mapInstExpr t doItIE

  let mapSVar t f =
    let rec doItIE (RInst {targs;sargs;args;rel}) = 
      let sargs' = List.map  (fun ts -> TS.instSVars ts f ) sargs in 
      let args' = List.map doItIE args in 
      RInst {targs=targs; sargs=sargs'; args=args';rel=rel}

    in
    mapInstExpr t doItIE

  let mapRel t f =
    let rec doItIE (RInst {targs;sargs;args;rel}) = 
      let rel' = f rel in 
      let args' = List.map doItIE args in 
      RInst {targs=targs; sargs=sargs;args=args';rel=rel'}

    in
    mapInstExpr t doItIE
end       
(*The Con.t must be replaced with the exact constructor*)
module StructuralRelation = struct (*<R, Tr m Const x , y -> r | Nil -> r ->*)
  type t = T of {id:RelId.t ; params : RelId.t list; mapp: ( Con.t * Ident.t list option * RelLang.term) list}
  (* Not allowed in Ocaml, this can always be replaces by application of T*)
  (*let newSR data = T data*)

  let conMapToString mapp =
    let conmap = "{" ^ (Vector.toString (fun (c,vlo,trm) ->
        let cstr = Con.toString c in 
        let vseq = match vlo with 
            None -> ""
          | Some vl -> Vector.toString Ident.name vl in 
        let trmstr = RelLang.termToString trm
        in
        cstr ^ vseq ^ " => " ^ trmstr
      ) mapp) ^ "}\n"
    in
    conmap

  let toString = fun (T{id;params;mapp}) -> 
    let relid = RelId.toString id in 
    let relstr = match List.length params with
        0 -> relid
      | _ -> relid ^ (List.fold_right (fun rid acc -> acc ^ " " ^ (RelId.toString rid)) params "" ) 
    in 

    let conmap = conMapToString mapp in 

    "relation (" ^ relstr ^ ") = " ^ conmap
end

module PrimitiveRelation = struct
  type def = Nullary of RelLang.expr
           | Nary of Ident.t * def 

  type t = T of {id: RelId.t; def:def}

  let rec defToString def = match def with 
      (Nullary rexpr) -> RelLang.exprToString rexpr
    |(Nary (v,def)) -> "\\"^(Ident.name v)^"."^(defToString def)

  let toString = fun (T {id;def}) -> "primitive relation "
                                     ^(RelId.toString id)^" = "^(defToString def)

  let rec applySubsts def substs = match def with 
      (Nary (v,def))  ->  Nary (v, applySubsts def substs)
    |(Nullary rexpr) ->  Nullary (RelLang.applySubsts substs rexpr)

  let rec instantiate def substs = 
      match def with  
    	| (Nary (v, def), arg :: args) ->  instantiate (def,args) ((arg,v)::substs)
    	| (def, [])  -> applySubsts def  substs
    	| (_,_)  -> failwith "Invalid primitive relation instantiation"
(* 
	let instantiate = fun (def, args) -> instantiate (def, Vector.toList args) []
 *)
  let symbase = "pv_" 

  let count = ref 0 

  let genVar = fun _ ->
    let id = symbase ^ (string_of_int (!count)) in 
    let _ = count := !count + 1 in 
    Var.fromString id

  let rec alphaRename x y = match (x,y) with 
      ((Nary (v,def)), substs) -> 
        let newV = genVar () in 
        Nary (newV, alphaRename def ((newV,v)::substs))
    | ((Nullary rexpr), substs) ->	Nullary (RelLang.applySubsts
                                              (substs) rexpr)

  let alphaRename =fun def -> alphaRename def []

end
module TyDBinds = struct
  module Key = struct
    type t = Var.t
    let layout = L.str <<  Var.toString (*Create a Layout Module*)
    let equal (v1, v2) = (Var.toString v1) = (Var.toString v2)
  end

  module Value = struct 
    type t = TyD.t
    let layout = fun x -> x 

  end 
  
  module TydMap = Applicativemap.ApplicativeMap (Key) (Value)
  type t = TydMap.t
  let mem = TydMap.mem 
  let find = TydMap.find
  let add = TydMap.add
  let empty = TydMap.empty




  exception KeyNotFound = TydMap.KeyNotFound

end

module TyDB = TyDBinds

module Predicate = 
struct
  exception RelPredicateException of string


  (*submodule *)
  module BasePredicate = struct
    type expr = Int of int
              | Bool of bool
              | Var of Var.t
    type t =  Iff of t * t
           | Eq of expr * expr

    let rec toString bp = match bp with
        Eq (Int i1,Int i2) -> (string_of_int i1) ^ " = " 
                              ^ (string_of_int i2)
      | Eq (Bool b1,Bool b2) -> (string_of_bool b1) ^ " = " 
                                ^ (string_of_bool b2)
      | Eq (Var v1, Var v2) -> (Var.toString v1) ^ " = " 
                               ^ (Var.toString v2)
      | Eq (Var v, Bool b) -> (Var.toString v) ^ " = " 
                              ^ (string_of_bool b)
      | Eq (Var v, Int i) -> (Var.toString v) ^ " = " 
                              ^ (string_of_int i)
                              
      | Iff (t1,t2) -> (toString t1) ^ " <=> " ^ (toString t2) 

    let varEq (v1, v2) = Eq (Var v1, Var v2)
    let varBoolEq (v,b) = Eq (Var v, Bool b)
    let varIntEq (v, i) = Eq (Var v, Int i)
    let rec  applySubst subst t = 
      let varSubst = varSubs subst in 
      match t with 
      |Eq (Var v1, Var v2) -> Eq (Var (varSubst v1), Var (varSubst v2))
      | Eq (Var v, e) -> Eq (Var (varSubst v), e)
      | Eq (e, Var v) -> Eq (e, Var  v)
      | Iff (t1,t2) -> Iff (t1, t2)


  end

  module RelPredicate = struct

    type expr = RelLang.expr
    type t = Eq of expr * expr
           | Sub of expr * expr
           | SubEq of expr * expr

    let toString rp = match rp with
        Eq (e1,e2) -> (RelLang.exprToString e1) ^ " = "
                      ^ (RelLang.exprToString e2)
      | Sub (e1,e2) -> (RelLang.exprToString e1) ^ " C "
                       ^ (RelLang.exprToString e2)
      | SubEq (e1,e2) -> (RelLang.exprToString e1) ^ " C= "
                         ^ (RelLang.exprToString e2)

    let exprMap rp f = match rp with 
        Eq (e1,e2) -> Eq (f e1, f e2)
      | Sub (e1,e2) -> Sub (f e1, f e2)
      | SubEq (e1,e2) -> SubEq (f e1, f e2)

    let applySubst subst t = exprMap t (RelLang.applySubsts ( [subst]))

    let mapTyD t f = 
      let g = RelLang.mapTyD in 
      let doIt = fun (x1,x2)  -> ((g x1 f), (g x2 f)) in

      match t with 
        Eq (x,y) -> let (x1, y1) = doIt (x, y) in  
          Eq (x1, y1)
      | Sub (x,y) -> 
          let (x1, y1) = doIt (x, y) in   
          Sub  (x1,y1)
      | SubEq (x,y) -> 
          let (x1, y1) = doIt (x, y) in   
          SubEq (x1,y1)

    let mapSVar t f = 
      let  g = RelLang.mapSVar in 
      let doIt = fun (x1,x2) -> (g x1 f, g x2 f) in 
      match t with 
        Eq (x,y) -> let (x1, y1) = doIt (x, y) in  
          Eq (x1, y1)
      | Sub (x,y) -> 
          let (x1, y1) = doIt (x, y) in   
          Sub  (x1,y1)
      | SubEq (x,y) -> 
          let (x1, y1) = doIt (x, y) in   
          SubEq (x1,y1)
  end 

  type t = True 
         |  False
         |  Base of BasePredicate.t 
         |  Rel of RelPredicate.t
         |  Exists of TyDBinds.t * t
         |  Not of t
         |  Conj of t * t
         |  If of t * t
         |  Iff of t * t
         |  Disj of t * t
         |  Dot of t * t
  (*needs to be rewritten when layout is defined *)          
 (*  let rec layout t = match t with 
      True -> L.str "true" 
    | False -> L.str "false" 
    | Base bp -> L.str  (BasePredicate.toString bp)
    | Rel rp -> L.str  (RelPredicate.toString rp )
    | Exists (binds,t) ->  L.str "exist"
    | Not t -> L.str "not" 
    | Conj (e1,e2) -> L.str ("Conj"  
    | Disj (e1,e2) -> L.str "Disj"  
    | If (e1,e2) -> L.str "If"
    | Iff (e1,e2) -> L.str "Iff"
    | Dot (e1,e2) -> L.str "Dot"
 *)
let rec  layout t = match t with
        True -> L.str "true" 
      | False -> L.str "false" 
      | Base bp -> L.str (BasePredicate.toString bp)
      | Rel rp -> L.str (RelPredicate.toString rp )
      | Exists (binds,t) -> let bindstr = List.fold_left (fun acc (k,v) -> (acc^ "Key ="^(Var.toString k)^" Value = "^(TyD.toString v))) "" binds in 
                                    L.seq [L.str "Exists";L.str bindstr ; L.str " such that ";layout t] 

      | Not t -> L.seq [L.str "not ("; layout t; L.str ")"]
      | Conj (e1,e2) -> L.align (L.separateLeft ([(layout e1); (
          layout e2)],"/\\ "))
      | Disj (e1,e2) -> L.align (L.separateLeft ([(layout e1); (
          layout e2)],"\\/ "))
      | If (e1,e2) -> L.align (L.separateLeft ([(layout e1); (
          layout e2)]," => "))
      | Iff (e1,e2) -> L.align (L.separateLeft ([(layout e1); (
          layout e2)]," <=> "))
      | Dot (e1,e2) -> L.align (L.separateLeft ([(layout e1); (
          layout e2)]," o "))



  let truee  _ = True
  let falsee _ = False 
  let isFalse e = match e with 
      False -> true | _ -> false
  let baseP p = Base p 
  let conj (t1, t2) = Conj (t1, t2)
  let conjR (t, r) = Conj (t, Rel r)
  let conJP (t,p) = Conj (t, Base p)

  let rec applySubst ((nw, ol) as subst) t = 
    match t with 
      True -> True
    | False -> False
    | Base bp ->
          let () = Printf.printf "%s" "\n Base Subst" in 
          Base (BasePredicate.applySubst subst bp)
    | Rel rp ->
         let () = Printf.printf "%s" "\n Rel Subst" in  
         Rel (RelPredicate.applySubst subst rp)
    | Exists (tyDB,t) -> 
        if (TyDBinds.mem tyDB ol)
        then let expstr = ("Attempted substitution "^(Ident.name nw)^"  on existentially quantified variable "^(Ident.name ol)) in 
            raise (RelPredicateException expstr) 
        else Exists (tyDB,applySubst subst t)
    | Not t -> Not (applySubst subst t )
    | Conj (t1,t2) -> Conj (applySubst subst t1, applySubst subst t2)
    | Disj (t1,t2) -> Disj (applySubst subst t1, applySubst subst t2)
    | If (t1,t2) ->
          If (applySubst subst t1, applySubst subst t2)
    | Iff (t1,t2) ->
        let () = Printf.printf "%s" "\n Iff Subst" in 
        Iff (applySubst subst t1, applySubst subst t2)
    | Dot (t1,t2) -> Dot (applySubst subst t1, applySubst subst t2)

  (* telescoped substitutions *)
  let rec applySubsts substs t = 
    List.fold_right (fun subst t ->
        applySubst subst t) substs t 

  let exists (tyb,t) = Exists (tyb,t)

  let dot (t1,t2) = Dot (t1,t2)


  let rec mapRP t f = match t with  
      Rel rp -> Rel (f rp)
    | Exists (tyDB,t) -> Exists (tyDB, mapRP t f)
    | Not t -> Not (mapRP t f)
    | Conj (t1,t2) -> Conj (mapRP t1 f, mapRP t2 f)
    | Disj (t1,t2) -> Disj (mapRP t1 f, mapRP t2 f)
    | If (t1,t2) -> If (mapRP t1 f, mapRP t2 f)
    | Iff (t1,t2) -> Iff (mapRP t1 f, mapRP t2 f)
    | Dot (t1,t2) -> Dot (mapRP t1 f, mapRP t2 f)
    | _ -> t 

  let rec mapTyD t f = match t with 
      Rel rp -> Rel (RelPredicate.mapTyD rp f)
    | Exists (tyDB,t) -> Exists (TyDBinds.TydMap.map  
                                   (fun (v,tyd) -> (v,f tyd)) tyDB, mapTyD t f)
    | Not t -> Not (mapTyD t f)
    | Conj (t1,t2) -> Conj (mapTyD t1 f, mapTyD t2 f)
    | Disj (t1,t2) -> Disj (mapTyD t1 f, mapTyD t2 f)
    | If (t1,t2) -> If (mapTyD t1 f, mapTyD t2 f)
    | Iff (t1,t2) -> Iff (mapTyD t1 f, mapTyD t2 f)
    | Dot (t1,t2) -> Dot (mapTyD t1 f, mapTyD t2 f)
    | _ -> t

  let mapsSVar t f = mapRP t (fun rp  -> RelPredicate.mapSVar rp f)

end

module P = Predicate
(*A module defining the refinement types *)
module RefinementType = 
struct 
  exception RefTyEx of string 
  type t = 
      Base of Var.t * TyD.t * Predicate.t 
    | Tuple of (Var.t * t) list
    | Arrow of (Var.t * t) * t
    (* Records are tuples with fixed bound var *)
  (* Needs extension for {'a | r} list *)


  let symbase = "v_" 
  let count = ref 0
  let genVar = fun _ ->
    let id = symbase ^ (string_of_int !count) in 
    let _  = count := !count + 1 in 
    Var.fromString id	
    (*currently there i sonly one type variable 'a*)
  let emptyVar () = Var.fromString ""    

  let rec fromTyD tyD = 
    let open TyD in 
    match tyD with 
      Tarrow (td1,td2) -> 
        Arrow ( (emptyVar (), fromTyD td1),
               fromTyD td2)
    | Ttuple tl  -> 
     let recRefTy = List.map (fun (td :TyD.t)->
        (emptyVar (), (fromTyD td))) tl in 
        Tuple recRefTy  
    | Ttop -> Base (genVar(), tyD, Predicate.truee())
    |  tyD -> Base (genVar(), tyD, Predicate.truee())
   
   let rec toTyD t = match t with
        Base (v,tdes,p) -> tdes
      | Tuple tv -> TyD.makeTtuple (List.map (fun (v,t) ->  
              let ty_desc_for_t = toTyD t in 
              ty_desc_for_t)  tv)
      | Arrow ((v1,t1),t2) -> TyD.makeTarrow ((toTyD t1), (toTyD t2))
     


(* 

  let rec layout rty = match rty with 
          Base(var,td,pred) -> L.seq [L.str ("{" ^ (Var.toString var)^ ":" ^ (TyD.toString td) ^ " | "); 
            Predicate.layout pred;L.str "}"]
        | Tuple tv -> L.vector (List.rev (List.map (fun (v,t) ->  L.seq [L.str (Var.toString v); L.str ":"; layout t]) tv))
        | Arrow ((v1,(Arrow (_,_) as t1)),t2) -> L.align (L.separateLeft (
            [L.seq 
                [L.str "("; L.str (Var.toString v1);L.str ":";
              layout t1; L.str ")"]; 
            layout t2]," -> "))
        | Arrow ((v1,t1),t2) -> L.seq [L.str (Var.toString v1); L.str ":"; layout t1 ; L.str "->" ; layout t2] 
             *)
  let rec layout rty = match rty with 
          Base(var,td,pred) -> L.seq [L.str ("{" ^ (Var.toString var)^ ":" ^ (TyD.toString td) ^ " | "); 
            Predicate.layout pred;L.str "}"]
        | Tuple tv -> L.vector (List.rev (List.map (fun (v,t) ->  L.seq [L.str (Var.toString v); L.str ":"; layout t]) tv))
        | Arrow ((v1,(Arrow (_,_) as t1)),t2) -> L.seq [
              L.seq[L.str "("; L.str (Var.toString v1); L.str ":"; layout t1; L.str ")"]; 
              L.seq [L.str "->"; layout t2]] 
        | Arrow ((v1,t1),t2) -> L.seq [L.str (Var.toString v1); L.str ":"; layout t1 ; L.str "->" ; layout t2] 


(* 

  let rec layout rty = match rty with 
          Base(var,td,pred) -> L.str "@refinementBase "
        | Tuple tv ->  L.str "@refinementTuple"
        | Arrow (_,_)-> L.str "@refinementArrow" *)
       
   let toString t = Layout.toString (layout t)    
    
  let  rec mapBaseTy t f = match t with
      Base (v,t,p) -> 
        (* let () = Printf.printf "%s" "\nBase substs" in 
         *)let (x,y,z) = f (v,t,p) in 
        Base (x,y,z)
    | Tuple tv -> 
     (*  let () = Printf.printf "%s" "\nTuple substs" in 
      *)   Tuple (List.map (fun (v,t) -> 
        (v,mapBaseTy t f)) tv)
    | Arrow ((v1,t1),t2) ->
     (*      let () = Printf.printf "%s" "\nArrow substs" in 
      *)      Arrow ((v1,mapBaseTy t1 f), 
                                   mapBaseTy t2 f)

  let mapTyD t f = mapBaseTy t (fun (v,t,p) -> 
      (v,f t, P.mapTyD p f)) 

  
  let rec applySubsts substs refty = 
	   mapBaseTy refty (fun (bv,t,pred) ->
	   (* if List.exists (fun(n,ol) -> 
          let () = Printf.printf "%s" ("Matching Replaced "^(Var.toString ol)) in 
          varStrEq (ol,bv)) substs 
				then  raise (RefTyEx ("Attempted substitution of bound var "))
				else  *)(bv,t,Predicate.applySubsts substs pred))

  let  alphaRenameToVar refty newbv = match refty with
      Base (bv,t,p) -> Base (newbv,t,
                             (Predicate.applySubst (newbv,bv) p))
    | _ -> raise (RefTyEx "alphaRename attempted on non-base type")

  let alphaRename refty = alphaRenameToVar refty (genVar())


  let exnTyp = fun _ -> Base (genVar(),TyD.makeTunknown (),
                              Predicate.falsee())

  let mapSVar t f = mapBaseTy t (fun (v,t,p) ->
     							(v,t, (Predicate.mapsSVar p f)))

     				
  let newLongVar = fun (var,fld) -> Var.fromString( 
        (Var.toString var)^"."^(Var.toString fld))
    
           (*
				     * Decomposes single tuple bind of form v ↦ {x0:T0,x1:T1} to
				     * multiple binds : [v.x0 ↦ T0, v.x1 ↦ T1]
				     *)
  
  let rec decomposeTupleBind (tvar , (Tuple refTyBinds) as tty) =
     					
              let  bindss = List.map (fun ((_,refTy) as refTyBind) -> 
      										match refTy with 
        											Tuple _ -> decomposeTupleBind refTyBind
      											| _ -> Vector.new1 refTyBind) 
     												refTyBinds in 
     				let binds = List.map (fun (v,ty) -> (newLongVar (tvar,v), ty)) (List.concat bindss) in 
     		     			binds
  

  let uncurry_Arrow (arrowty) =
    let Arrow ((_, frstargTy), res) = arrowty in  
      
      let rec get_params ty params resTy  = 
        match ty with
          | Arrow ((arg, argTy) as argBind, remainingTy)  ->
            let params' = (argBind :: params) in 
                get_params remainingTy params' resTy 

          | _ -> (params, ty)
    in 
    let (parametersTyBind, resTy ) =  get_params arrowty [] res in
     (List.rev parametersTyBind, resTy)
 
end

module RefTy = RefinementType 


module ParamRefType =  struct
  type t = T of {params : (RelId.t * SimpleProjSort.t) list; refty : RefinementType.t}

  let layout (T {params; refty}) = 
    let typedParamLyt (r,sprojty) = L.str  
        ((RelId.toString r) ^ " :: " ^
         (SPS.toString sprojty)) in 
    let paramslyt = 
      let consFun = fun x xs -> x :: xs in 
      List.fold_right (consFun) [] (List.map typedParamLyt params) in 
    let reftylyt = RefinementType.layout refty in 
    L.seq [(L.seq paramslyt); L.str ". "; reftylyt]

  let parametrize (sortedParams,refTy) = T {params=sortedParams; refty=refTy}

  let mapTyD (T {params;refty}) f = 
    let params' = List.map (fun (r,sps) -> (r, SPS.mapTyD sps f)) params  in 
    let refty' = RefTy.mapTyD refty f in
    T {params=params'; refty=refty'}

end

module PRf = ParamRefType

module RefinementSortScheme = struct
  type t = T of {svars: SVar.t list; prefty : ParamRefType.t }

  let layout (T {svars; prefty}) =  
    let svlyt = 
      let consFun  = fun x xs -> x :: xs in 
      List.fold_right (consFun) [] (List.map (fun sv -> L.str (SVar.toString sv)) svars) in 
    let prflyt = ParamRefType.layout prefty in 
    L.seq [(L.seq svlyt); L.str ". "; prflyt]

  let fromRefTy = fun refTy -> T {svars = empty(); prefty = ParamRefType.T {params=empty(); refty=refTy}}

  let toRefTy = fun (T {prefty = ParamRefType.T {refty;_} ;_}) -> refty

  let generalize = fun (svars,prefty) -> T {svars=svars; prefty=prefty}

  let mapTyD (T {svars;prefty}) f = T {svars=svars;prefty=PRf.mapTyD prefty f}

end

module RefSS = RefinementSortScheme

module RefinementTypeScheme =
struct
  type t = T of {tyvars : Tyvar.t list;
                 refss : RefinementSortScheme.t;
                 (* ICFP taking its toll What does this signify *)
                 isAssume : bool}

  let generalizeRefTy = fun (tyvars, refTy) ->
    T {tyvars = tyvars; refss = RefSS.fromRefTy refTy; 
       isAssume = false}

  let generalize = fun (tyvars, refss) ->
    T {tyvars = tyvars; refss = refss; isAssume = false}

  let generalizeAssump = fun (tyvars, refss, isAssume) ->
    T {tyvars = tyvars; refss = refss; isAssume = isAssume}

  let isAssumption = fun(T {isAssume;_}) -> isAssume

  let specialize = fun (T {tyvars;refss;_}) ->
    refss

  let specializeRefTy = fun (T {tyvars;refss;_}) ->
    RefSS.toRefTy refss

  let layout (T {tyvars;refss;isAssume}) =
    let flaglyt = (if isAssume then L.str "Assumption: " else
                     L.empty) in 
    let tyvlyt =
      let consFun = fun x xs -> x :: xs in  
      List.fold_right (consFun) [] (List.map (fun tyv ->
          L.str (Tyvar.toString tyv)) tyvars) in 
    let refsslyt = RefinementSortScheme.layout refss in 
    L.seq ( [flaglyt;( L.seq tyvlyt);refsslyt])

  let toString t = Layout.toString (layout t) 

  let instantiate (T{tyvars;refss;_},tydvec) =
    let len = List.length in 
    let _ = assert (len tyvars = len tydvec) in 
    let tyvmap = Vector.zip tydvec tyvars
            (*
		           * It is possible that we encounter a tyvar
		           * that is not generalized in this RefTyS.
		           * We do not panic.
		           *)
    in
    RefSS.mapTyD refss (TyD.instantiateTyvars tyvmap)
end

module  RefTyS = RefinementTypeScheme

module RelSpec = struct 
  module TypeSpec =
    struct
      type t = T of {isAssume : bool;
                   name:Var.t;
                   params: RelId.t list;
                   refty : RefinementType.t}
      let layout = fun (T {name=var;params;refty;_}) -> 
                      L.seq [
       				        	L.str ((Var.toString var) ^ " : ");
       				        	L.str (Vector.toString RelId.toString params);
       				        	RefinementType.layout refty]
       let toString t = L.toString (layout t)                 
    end
  
  type t = T of {reldecs : StructuralRelation.t list;
                 primdecs : PrimitiveRelation.t list;
                 typespecs : TypeSpec.t list}
   let layout = fun (T {reldecs;primdecs;typespecs;_}) ->
      				      let srs = Vector.toString (StructuralRelation.toString) (reldecs) in 
      				      let prs = Vector.toString (PrimitiveRelation.toString) (primdecs) in 
      				      let tslyt = L.align (Vector.toListMap (typespecs,
      				          TypeSpec.layout))
      				      in
      				        L.align [L.str srs; L.str prs; tslyt]

   let toString t = L.toString (layout t)   

   let mk_empty_relspec () = T {reldecs= [];
                 primdecs = [];
                 typespecs =[]}                
end

module Bind = struct
  exception BindException of string 

  type transformer = Fr of Var.t list * RelLang.expr 
  type expr = Expr of {ground : RelId.t * TyD.t list * Var.t;
                       fr : transformer}

  type abs = Abs of Var.t * expr

  type def = 
      Def of  {tyvars : Tyvar.t list; params : RelId.t list; abs : abs}
    | BogusDef

  let symbase = "v_"

  let count = ref 0

  let genVar = fun _ -> 
    let id = symbase ^ (string_of_int (!count)) in 
    let _ = count := !count + 1
    in
    Var.fromString id 

  let frToString (Fr (vs,rexpr)) =
    let vsStr = Vector.toString Var.toString vs in 
    let reStr = RelLang.exprToString rexpr
    in
    "\\"^vsStr^". "^reStr

  let bindExprToString (Expr {ground = (r,tydv,x);fr}) =
    let tydStr = Vector.toString TyD.toString tydv in 
    let vStr = Var.toString x in 
    let gStr = (RelId.toString r)^" "^tydStr^" ("^vStr^")" in 
    let frStr = frToString fr in

    "bind ("^gStr^","^frStr^")"

  let absToString (Abs (v,bindex)) = 
    let bStr = bindExprToString bindex in
    "\\"^(Var.toString v)^". "^bStr

  let defToString definition = 
      match definition with  
     | (Def {tyvars;params=rs;abs}) ->
      let tyvStr = Vector.toString Tyvar.toString tyvars in 
      let absStr = absToString abs in 
      let rsStr = fun _ -> Vector.toString RelId.toString rs in
      
        (match Vector.length rs with 
          0 -> absStr
          | _ -> (tyvStr ^" \\"^(rsStr ())^". "^absStr))


    | BogusDef -> "-- NA --"

  let groundRelTyS pts = 
    let PTS.T {tyvars;sortscheme = PSS.T {sort = ProjSort.T 
                                              {paramsorts; sort=groundSort}; _}} = pts in 
    (* SVar to Tyvar map *)
    let svarMap = List.map (fun (SPS.ColonArrow (a, TS.Tuple [TS.S t])) -> (t,a)) paramsorts in 
    let mapSVar = fun t -> 
      match Vector.peekMap (svarMap, fun (t',a) -> if (SVar.eq t t') then Some a else None) with
        Some a -> a 
      | None -> raise (BindException "SVar impossible case") in 
    let SPS.ColonArrow (tyd,TS.Tuple tts) = groundSort in
    let tyds = List.map (fun tt -> match tt with  
          TS.S t -> TS.T ( mapSVar t )
        | _ -> tt) tts in 

    PTS.simple (tyvars, SPS.ColonArrow (tyd, TS.Tuple tyds))


  let makeGroundDef (params,rterm) : RelLang.term = 
    let open RelLang in 
    let  empty = fun _ -> Vector.new0 () in 
    let isParam = fun rid -> List.exists (fun p ->
        RelId.toString rid = RelId.toString p) params in 
    let rec doItExp exp = match exp with
        U (e1,e2) -> U (doItExp e1, doItExp e2)
      | X (e1,e2) -> X (doItExp e1, doItExp e2)
      | D (e1,e2) -> D (doItExp e1, doItExp e2)
      | R (RInst {rel;targs;sargs;args},x) -> if isParam rel 
          then RelLang.rId x
          else (R (RInst {rel=rel; targs=targs; sargs=empty();
                          args=empty()},x))
      | _ -> exp

    in
    match rterm with  
      Expr exp -> Expr ( doItExp exp )
    | Star (RInst {rel;targs;sargs;args}) -> Star (
        RInst {rel=rel; targs=targs; sargs=sargs; 
               args=empty()} )


  let makeBindDef (id,params,pts) : def =
    let PTS.T {sortscheme = PSS.T {sort = PS.T {paramsorts = 
                                                  paramSorts; sort=groundSort}; _}; tyvars} = pts in 
    (* SVar to RelId map *)
    let svarMap = List.map2 (fun (SPS.ColonArrow (_,TS.Tuple [TS.S t])) rid -> (t,rid)) paramSorts params  in 
    let mapSVar = fun t -> match Vector.peekMap (svarMap,  
                                                 fun (t',rid) -> if (SVar.eq t t') then Some rid else None) with
      Some rid -> rid 
    | None -> raise (BindException "SVar impossible case") in 
    let SPS.ColonArrow (_,TS.Tuple tts) = groundSort in 

(*     let () = Printf.printf "%s" ("\n Size of tts "^(string_of_int (List.length tts))) in 
 *)
    let (bvs,rApps) = Vector.unzip (List.map ( fun tt -> 
        let v = genVar () in 
        match tt with 
          TS.T tyd -> (v,RelLang.rId v)
        | TS.S t -> (v, RelLang.appR (mapSVar t, empty (), v))
      ) tts) in 
(*     let () = List.iter (fun rApp -> Printf.printf "%s" ("\n Rapp "^RelLang.exprToString rApp)  
               ) rApps in 
 *)
    let Some xexpr = List.fold_right 
        (fun rApp xop -> match xop with 
          None -> (*let () = Printf.printf "%s" "@ this node None " in
              let () = Printf.printf "%s" ("\n Rapp "^RelLang.exprToString rApp) in 
               *)
               Some rApp 
        | Some xexpr -> (* let () = Printf.printf "%s" "@ this node Some " in 
              let () = Printf.printf "%s" ("\n Rapp "^RelLang.exprToString rApp) in 
              let () = Printf.printf "%s" ("\n xexpr "^RelLang.exprToString xexpr) in  *)
              Some (RelLang.crossprd (rApp,xexpr))
        | _ -> let () = Printf.printf "%s" "@ this node Some " in raise (SpecLangEx "Unimpl :: xepr case unhandled ") 
        )rApps None 

     in 
   (*  let xexpr = match  xexpr with
      | Some s -> s 
      | None  -> None 
    in    
    *) let bv = genVar () in 
    let fr = Fr (bvs,xexpr) in 
    let targs = List.map (TyD.makeTvar) tyvars in 
    let bindex = Expr {ground = (id,targs,bv); fr=fr} in 
    let bindabs = Abs (bv,bindex)
    in
    Def {tyvars=tyvars; params=params; abs=bindabs}

  let instantiate (Def {tyvars;params;abs},tydvec,ridvec) : abs =
    let tsubsts = try Vector.zip tydvec tyvars with 
        _ -> raise (BindException  "Bind : tyvar inst error") in 
    let rmap = try  Vector.zip params ridvec with 
      | _ -> raise (BindException "Bind : params inst error") in 
    let err = fun _ -> raise (BindException "Bind: inst error") in 
    let mapt = TyD.instantiateTyvars tsubsts in  
    let mapr = mkMapper rmap RelId.equal err in 
    let  Abs (bv,Expr {ground=(gr,targs,_); fr=Fr (xs,rexpr)}) = abs in 
    let targs' = List.map  mapt targs in 
    let ground' = (gr,targs',bv) in 
    let rexpr' = RelLang.mapRel rexpr mapr in 
    let expr' = Expr {ground=ground'; fr = Fr (xs,rexpr')} in 
    let abs' = Abs (bv, expr') in 
    abs'

  let instantiate (BogusDef, _, _) = raise (BindException "Cannot instantiate \
                                                           								\ bogus bind def")

  let fromAbs (abs:abs) : def = Def {tyvars=empty (); params=empty (); abs=abs}
end
