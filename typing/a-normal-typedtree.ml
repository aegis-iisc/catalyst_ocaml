(*Ashish*)


open Asttypes
open Types

type partial = Partial | Total 

type attribute = Parsetree.attribute
type attributes = attribute list


(* Core Language *)

module Val = struct
        type atom = 
                | Const of constant
                | Var of (Ident.t * string loc)
                | Any

        type t = 
                | Atom of atom 
                | Tuple of atom list 
                | Record of label_description * atom list 
end 
type pattern =
           { pat_desc: pattern_desc;
               pat_loc: Location.t;
              pat_extra : (pat_extra * Location.t * attributes) list;
                pat_type: type_expr;
                mutable pat_env: Env.t;
                  pat_attributes: attributes;
          }
  and pat_extra = 
        | Tpat_constraint of core_type 

        | Tpat_type of Path.t * Longident.t loc

        | Tpat_unpack 

and pattern_desc = 
        | Tpat_any 
        | Tpat_var of (Ident.t * string loc)
        | Tpat_alias of pattern * Ident.t * string loc
		| Tpat_constant of Val.atom 
		| Tpat_tuple of Val.t
        | Tpat_construct of 
                Longident.t loc * constructor_description * pattern list
 		| Tpat_variant of label * pattern option * row_desc ref
 		| Tpat_record of
      			(Longident.t loc * label_description * pattern) list * closed_flag
  		| Tpat_array of pattern list
  		| Tpat_or of pattern * pattern * row_desc option
	    | Tpat_value of Val.t
    	   
and expression = 
        {
                exp_desc : expression_desc;
                exp_loc : Location.t;
                exp_extra : (exp_extra * Location.t * attributes) list;
                exp_type : type_expr;
                exp_env: Env.t;
                exp_attributes : attributes;
                


        }

and exp_extra = 
        | Texp_constraint of core_type 
        | Texp_coerce of core_type option * core_type 
        | Texp_open of override_flag * Path.t * Longident.t loc * Env.t
         (** let open[!] M in    [Texp_open (!, P, M, env)]
          *                                 where [env] is the environment after opening [P]
          *                                          *)
        | Texp_poly of core_type option
                   (** Used for method bodies. *)
       | Texp_newtype of string
                             (** fun (type t) ->  *)
                             
and expression_desc = 
	Texp_ident of Path.t * Longident.t loc * Val.t
	| Texp_constant of constant
	| Texp_let of rec_flag * value_binding list * expression
	| Texp_function of arg_label  * case list * partial
	| Texp_apply  of Val.atom * (arg_label * Val.t option) list
	| Texp_match of Val.t * case list * case list * partial 
	| Texp_try of Val.t * case list 
	| Texp_tuple of Val.t list
	| Texp_construct of  Longident.t loc * constructor_description * Val.t list
        (** C                []
            C E              [E]
            C (E1, ..., En)  [E1;...;En]
         *)
  | Texp_variant of label * Val.t option
  | Texp_record of
      (Longident.t loc * label_description * Val.t) list *
        Val.t option
  | Texp_field of Val.t * Longident.t loc * label_description
  | Texp_setfield of
      Val.t * Longident.t loc * label_description * Val.t
  | Texp_array of Val.t list
  | Texp_ifthenelse of Val.t * Val.t * Val.t option
  | Texp_sequence of Val.t * Val.t
  | Texp_while of Val.t * Val.t
  
and meth = 
    Tmeth_name of string
  | Tmeth_val of Val.atom

and case = {
	
	c_lhs : pattern;
	c_guard : Val.t option ;
	c_rhs : Val.t ;
}

(*Skipping the class and other Object oriented feautures*)
(*Skipping the Module language*)

and value_binding =
  {
    vb_pat: pattern;
    vb_expr: Val.t;
    vb_attributes: attributes;
    vb_loc: Location.t;
  }

and signature = {
  sig_items : signature_item list;
  sig_type : Types.signature;
  sig_final_env : Env.t;
}

and signature_item =
  { sig_desc: signature_item_desc;
    sig_env : Env.t; (* BINANNOT ADDED *)
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of value_description
  | Tsig_type of rec_flag * type_declaration list
  | Tsig_typext of type_extension
  | Tsig_exception of extension_constructor
  | Tsig_module of module_declaration
  | Tsig_recmodule of module_declaration list
  | Tsig_attribute of attribute

and core_type =
  { mutable ctyp_desc : core_type_desc;
      (** mutable because of [Typeclass.declare_method] *)
    mutable ctyp_type : type_expr;
      (** mutable because of [Typeclass.declare_method] *)
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t;
    ctyp_attributes: attributes;
   }

and core_type_desc =
    Ttyp_any
  | Ttyp_var of string
  | Ttyp_arrow of arg_label * core_type * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_constr of Path.t * Longident.t loc * core_type list
  | Ttyp_object of (string * attributes * core_type) list * closed_flag
  | Ttyp_class of Path.t * Longident.t loc * core_type list
  | Ttyp_alias of core_type * string
  | Ttyp_variant of row_field list * closed_flag * label list option
  | Ttyp_poly of string list * core_type


and value_description =
  { val_id: Ident.t;
    val_name: string loc;
    val_desc: core_type;
    val_val: Types.value_description;
    val_prim: string list;
    val_loc: Location.t;
    val_attributes: attributes;
    }

and type_declaration =
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

and type_kind =
    Ttype_abstract
  | Ttype_variant of constructor_declaration list
  | Ttype_record of label_declaration list
  | Ttype_open

and label_declaration =
    {
     ld_id: Ident.t;
     ld_name: string loc;
     ld_mutable: mutable_flag;
     ld_type: core_type;
     ld_loc: Location.t;
     ld_attributes: attributes;
    }

and constructor_declaration =
    {
     cd_id: Ident.t;
     cd_name: string loc;
     cd_args: constructor_arguments;
     cd_res: core_type option;
     cd_loc: Location.t;
     cd_attributes: attributes;
    }

and constructor_arguments =
  | Cstr_tuple of core_type list
  | Cstr_record of label_declaration list

and type_extension =
  {
    tyext_path: Path.t;
    tyext_txt: Longident.t loc;
    tyext_params: (core_type * variance) list;
    tyext_constructors: extension_constructor list;
    tyext_private: private_flag;
    tyext_attributes: attributes;
  }
(* Auxiliary functions over the a.s.t. 

val iter_pattern_desc: (pattern -> unit) -> pattern_desc -> unit
val map_pattern_desc: (pattern -> pattern) -> pattern_desc -> pattern_desc

val let_bound_idents: value_binding list -> Ident.t list
val rev_let_bound_idents: value_binding list -> Ident.t list

val let_bound_idents_with_loc:
    value_binding list -> (Ident.t * string loc) list

(** Alpha conversion of patterns *)
val alpha_pat: (Ident.t * Ident.t) list -> pattern -> pattern

val mknoloc: 'a -> 'a Asttypes.loc
val mkloc: 'a -> Location.t -> 'a Asttypes.loc

val pat_bound_idents: pattern -> Ident.t list
*)

(* Auxiliary functions over the a.s.t. *)

let iter_pattern_desc f = function
  | Tpat_construct(_, cstr, patl) -> List.iter f patl
  | Tpat_any -> ()
  | Tpat_var _ -> ()
  
let map_pattern_desc f d =
  match d with
  | Tpat_construct (lid, c,pats) ->
      Tpat_construct (lid, c, List.map f pats)
  | Tpat_any -> Tpat_any
  

let idents = ref([]: (Ident.t * string loc) list)

let rec bound_idents pat =
  match pat.pat_desc with
  | Tpat_var (id,s) -> idents := (id,s) :: !idents
  | Tpat_alias(p, id, s ) ->
      bound_idents p; idents := (id,s) :: !idents
  | Tpat_or(p1, _, _) ->
      (* Invariant : both arguments binds the same variables *)
      bound_idents p1
  | d -> iter_pattern_desc bound_idents d

let pat_bound_idents pat =
  idents := [];
  bound_idents pat;
  let res = !idents in
  idents := [];
  List.map fst res

let rev_let_bound_idents_with_loc bindings =
  idents := [];
  List.iter (fun vb -> bound_idents vb.vb_pat) bindings;
  let res = !idents in idents := []; res

let let_bound_idents_with_loc pat_expr_list =
  List.rev(rev_let_bound_idents_with_loc pat_expr_list)

let rev_let_bound_idents pat = List.map fst (rev_let_bound_idents_with_loc pat)
let let_bound_idents pat = List.map  fst (let_bound_idents_with_loc pat)

let alpha_var env id = List.assoc id env

let rec alpha_pat env p = match p.pat_desc with
| Tpat_var (id, s) -> (* note the ``Not_found'' case *)
    {p with pat_desc =
     try Tpat_var (alpha_var env id, s) with
     | Not_found -> Tpat_any}
| d ->
    {p with pat_desc = map_pattern_desc (alpha_pat env) d}

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc
