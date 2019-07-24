(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

open Parsetree
open Asttypes
open Format

open Misc

module C = Common

exception NormalizationFailure of expression * Location.t * string


let wrap_printable exp = (Ptop_def([{pstr_desc = (Pstr_eval (exp, [])); pstr_loc = Location.none}])) 
let dummy = Location.none
let dummy_attrs = [] 

let set_desc e d = {pexp_loc = e.pexp_loc; pexp_desc = d; pexp_attributes= e.pexp_attributes}

let ncnt = ref 0 
let fresh_name_s () = incr ncnt; "_'" ^ (string_of_int !ncnt) 
let fresh_name () = Longident.parse (fresh_name_s ())
 
let li_flatten li = String.concat "." (Longident.flatten li) 
(*returns the 'a Location for an input of type 'a*)
let get_location txt = {
                  txt =  txt;
                  loc = Location.none;
                }

let get_back_base (ls : (Longident.t Asttypes.loc * Parsetree.expression option * Location.t) list) :
(Longident.t  * Parsetree.expression option * Location.t) list =
(*List of (Longident.t Asttypes.loc * Parsetree.expression option * Location.t)*) 
  List.map (fun (l1, e, l2) -> (l1.txt, e, l2)) ls 

let get_extended (ls : (Longident.t * Parsetree.expression option * Location.t) list) :
(Longident.t Asttypes.loc * Parsetree.expression option * Location.t) list =
  List.map (fun (l1, e, l2) -> (get_location l1, e, l2)) ls 

let is_op exp nm = 
  match exp.pexp_desc with
    | Pexp_ident(id) ->
        li_flatten id.txt = nm
    | _ -> false

let is_mult exp = is_op exp "*"
let is_div exp = is_op exp "/"
let is_deref e = is_op e "!"
let is_assign e = is_op e ":="
               
let is_const exp =
  match exp.pexp_desc with
    | Pexp_constant _ ->
        true
    | _ -> false

let is_function exp =
  match exp.pexp_desc with
    | Pexp_function _  ->
        true
    | _ -> false

let is_const_div exp = 
  match exp.pexp_desc with 
    Pexp_apply(e1, es) ->
      let es = List.map (fun (_, e) -> e) es in
      let div = is_div e1 in
        if div then is_const (List.nth es 1) else false 
    | _ -> false
      
let is_const_mult exp =
  match exp.pexp_desc with
    Pexp_apply(e1, es) ->
      let es = List.map (fun (_, e) -> e) es in
      let mult = is_mult e1 in
        if mult then is_const (List.nth es 1) || is_const (List.hd es)
        else false
    | _ -> false

let desugar_deref e =
  let desc = match e.pexp_desc with
             | Pexp_apply (_, [(_, e)]) -> Pexp_field (e, 
              {
                  txt = C.s_to_l "contents";
                  loc = Location.none;
                })
             | _ -> assert false in
    set_desc e desc

let desugar_assign e =
  let desc = match e.pexp_desc with
             | Pexp_apply (_, [(_, e); (_, e')]) -> Pexp_setfield (e, {
                  txt = C.s_to_l "contents";
                  loc = Location.none;
                }, e')
             | _ -> assert false in
    set_desc e desc

let mk_argpat x = {ppat_desc = Ppat_var (get_location x); ppat_loc = dummy; ppat_attributes = dummy_attrs }
let mk_let_lit r pes e2 = Pexp_let(r, pes, e2)
let mk_let r x e1 e2 =
    let patt = mk_argpat x in 
    let expp = e1 in 
    let attrr = dummy_attrs in 
    let locc = dummy in
    let v_binding = {
      pvb_pat = patt;
      pvb_expr = expp;
      pvb_attributes = attrr;
      pvb_loc= locc;
  }  in  
    mk_let_lit r [v_binding] e2
let mk_let_lbl r x e1 e2 = mk_let r (li_flatten x) e1 e2
let mk_apply e1 es = Pexp_apply(e1, (List.map (fun e -> (Asttypes.Labelled "", e)) es))
let mk_ident id = Pexp_ident(id)
let mk_function lbl elbl arg_pat sube = Pexp_fun(lbl, elbl, arg_pat, sube)

let mk_array es = Pexp_array(es) 
let mk_tuple es = Pexp_tuple(es)
let mk_sequence e1 e2 = Pexp_sequence(e1, e2)
let mk_ifthenelse e1 e2 e3 = Pexp_ifthenelse(e1, e2, Some e3)
let mk_field e s = Pexp_field(e, s)
let mk_setfield e s e' = Pexp_setfield(e, s, e')
let mk_record es = Pexp_record(es, None)
let mk_assert e = Pexp_assert(e)
let mk_match e pel = Pexp_match(e, pel)
let mk_construct l e  = Pexp_construct(l, Some e)
let mk_constraint c e = Pexp_constraint(e, c)

let mk_dummy desc loc = {pexp_desc = desc; pexp_loc = loc; pexp_attributes = dummy_attrs}

let mk_dum_ident id loc = mk_dummy (mk_ident id) loc
let mk_ident_loc id loc = {pexp_desc = mk_ident id; pexp_loc = loc; pexp_attributes = dummy_attrs}

let un = mk_dummy (Pexp_construct (get_location (Longident.Lident "()"), None)) dummy

let mk_alias p =
  Ppat_alias (p, get_location (fresh_name_s ()))

let elim_anys p =
  let rec elim_rec p =
    let np = 
      match p.ppat_desc with
      | Ppat_any                     -> Ppat_var (get_location (fresh_name_s ()))
      | Ppat_var _ | Ppat_constant _ -> p.ppat_desc
      | Ppat_alias (p', x)           -> Ppat_alias (elim_rec p', x)
      | Ppat_constraint (p, t)       -> Ppat_constraint (elim_rec p, t)
      | Ppat_or (p1, p2)             -> Ppat_or (elim_rec p1, elim_rec p2)
      | Ppat_tuple (pl)              ->
          mk_alias ({ppat_desc = Ppat_tuple (List.map elim_rec pl); ppat_loc = p.ppat_loc; ppat_attributes = dummy_attrs})
      | Ppat_construct (id, Some ({ppat_desc = Ppat_tuple (pl)} as pt)) ->
          mk_alias {ppat_desc = Ppat_construct (id, Some ({pt with ppat_desc = Ppat_tuple (List.map elim_rec pl)}));
                    ppat_loc = p.ppat_loc; ppat_attributes = p.ppat_attributes}
      | Ppat_construct (id, p') -> 
          let p' = match p' with Some {ppat_desc = Ppat_any} -> p' | Some p -> Some (elim_rec p) | None -> None in
            mk_alias {ppat_desc = Ppat_construct(id, p'); ppat_loc = p.ppat_loc; ppat_attributes = p.ppat_attributes}
      | p -> assert false in
    {ppat_desc = np; ppat_loc = p.ppat_loc; ppat_attributes = p.ppat_attributes} in
  elim_rec p

let (<+>) f g x = f (g x)
let rec expand_or_pats p =
  match p.ppat_desc with
    | Ppat_any                        -> [p]
    | Ppat_var _ | Ppat_constant _    -> [p]
    | Ppat_construct (_, None)     -> [p]
    | Ppat_alias (p', x)              -> [p' |> expand_or_pats |> fun p' -> {p with ppat_desc = Ppat_alias (List.nth p' 0, x)}]
    | Ppat_constraint (p', t)         -> [p' |> expand_or_pats |> fun p' -> {p with ppat_desc = Ppat_constraint (List.nth p' 0, t)}]
    | Ppat_or (p1, p2)                -> List.concat [expand_or_pats p1 ; expand_or_pats p2]
    | Ppat_tuple (pl)                 -> pl
    | Ppat_construct (id, Some p') -> [p' |> expand_or_pats |> fun p' -> {p with ppat_desc = Ppat_construct (id, Some (List.nth p' 0))}]
    | _                               -> assert false

let resolve_in_exp_when f ls =
  let (lbl, lex, loc) = List.hd ls in
  match lex with
    | Some lex when f lex -> (lex, List.tl ls)
    | _ -> (mk_ident_loc lbl loc, ls) 

let resolve_in_exp = resolve_in_exp_when (fun e -> true) 
let resolve_in_exp_never = resolve_in_exp_when (fun e -> false)

let normalize exp =
 let rec norm_out exp =
    let rw_expr desc = {pexp_desc = desc; pexp_loc = exp.pexp_loc; pexp_attributes = exp.pexp_attributes} in
    let wrap r b (lbl, a, _) = 
      match a with
          Some a -> mk_let_lbl r lbl a (mk_dummy b exp.pexp_loc)
          | None -> b
    in
    let proc_list es skel =
     let lss = List.map norm_in es in
     let (lbls, lss) = List.fold_right (fun ls (ess, lss) ->
                                        let (e, ls) = resolve_in_exp_when is_const ls in
                                          (e::ess, ls::lss)) lss ([], []) in
     let init = skel lbls in
      rw_expr (List.fold_left (wrap Nonrecursive) init (List.rev ( get_back_base (List.concat lss))))
    in
    let loc = exp.pexp_loc in

    match exp.pexp_desc with
     | Pexp_constant(_) 
     | Pexp_construct(_, None) ->
         exp
     | Pexp_construct(cstrdesc, Some e) ->
         let ls = norm_in e in
         let (inex, ls) = resolve_in_exp ls in
         let init = mk_construct cstrdesc inex  in
          rw_expr (List.fold_left (wrap Nonrecursive) init (get_back_base ls))
     | Pexp_constraint(e, c1) ->
         rw_expr (mk_constraint c1 (norm_out e))
     | Pexp_ident(_) ->
        exp
     | Pexp_fun(lbl, elbl, arg, e) ->
        rw_expr (mk_function lbl elbl arg (norm_out e))
     | Pexp_let(Recursive, pes, e2) ->
        (* we can assume more or less that all recursive ands are 
         * binds of mutually recursive functions, so we won't even try
         * to norm_in them *)
        let pes = 

          List.map (fun (vb) -> {
             pvb_pat = vb.pvb_pat;
            pvb_expr=  norm_out vb.pvb_expr;
            pvb_attributes = vb.pvb_attributes;
            pvb_loc = vb.pvb_loc;}
             ) pes in
          rw_expr (mk_let_lit Recursive pes (norm_out e2))
     (*|  Pexp_let(Nonrecursive, pes, e2) ->
         let mk_lbl (vb) = 
           let ls = norm_in vb.pvb_expr in
           let (lbl, _, lo) = List.hd ls in
           let lbl = mk_ident_loc lbl lo in
             (lbl, ls) in
         
         let lbss = List.map mk_lbl pes in
         let pes = List.map2 (fun (p, e) (lbl, ls) -> (p, lbl)) pes lbss in
         let lss = List.map (fun (lbl, ls) -> ls) lbss in
         let init = mk_let_lit Nonrecursive pes (norm_out e2) in 
           rw_expr (List.fold_left (wrap Nonrecursive) init (List.concat (List.rev lss))) *)
     | Pexp_apply(e1, es) ->
        let f = norm_in e1 in
        let (flbl, _, lo) = List.hd f in 
        let es = List.map (fun (_, e) -> e) es in
        let lss = List.map norm_in es in
        let ts = List.map (fun ls -> let (lbl, _, lo) = List.hd ls in mk_ident_loc lbl lo) lss in
        (* hack for constant div *)
        let ts = if is_const_div exp || is_const_mult exp then
                    let e_n1 = List.hd ts in
                    let e_n2 = List.nth ts 1 in
                    let e_o1 = List.hd es in
                    let e_o2 = List.nth es 1 in
               [if is_const e_o1 then e_o1 else e_n1;
                if is_const e_o2 then e_o2 else e_n2]
               else ts in
        let init = mk_apply (mk_ident_loc flbl lo) ts in
        let ls = List.concat (List.rev (f::lss)) in
          if is_deref e1 then norm_out (desugar_deref exp) else
          if is_assign e1 then norm_out (desugar_assign exp) else
          rw_expr (List.fold_left (wrap Nonrecursive) init (get_back_base ls))  
     | Pexp_ifthenelse(e1, e2, Some e3) ->
        let (e_b, b) = resolve_in_exp_never (norm_in e1) in 
        let init = mk_ifthenelse e_b (norm_out e2) (norm_out e3) in
         rw_expr (List.fold_left (wrap Nonrecursive) init (get_back_base b))
     | Pexp_ifthenelse(e1, e2, None) ->
         norm_out (rw_expr (Pexp_ifthenelse(e1, e2, Some un)))
     | Pexp_tuple(es) ->
        proc_list es mk_tuple
     | Pexp_array(es) ->
        proc_list es mk_array
     | Pexp_sequence(e1, e2) ->
        rw_expr (mk_sequence (norm_out e1) (norm_out e2))
     | Pexp_assert(e) ->
        let c = norm_in e in 
        let (inner, c) = resolve_in_exp_never c in
        let init = mk_assert inner in
          rw_expr (List.fold_left (wrap Nonrecursive) init (get_back_base c))
     | Pexp_field(e, s) ->
        let ls = norm_in e in
        let (lbl, _, lo) = List.hd ls in
        let init = mk_field (mk_ident_loc lbl lo) s in
          rw_expr (List.fold_left (wrap Nonrecursive) init ( get_back_base ls))
     | Pexp_setfield(e, s, e') ->
        let (lhs, lls) = resolve_in_exp_never (norm_in e) in
        let (rhs, rrs) = resolve_in_exp (norm_in e') in
        let init = mk_setfield lhs s rhs in
          rw_expr (List.fold_left (wrap Nonrecursive) init ( get_back_base (rrs @ lls)))
     | Pexp_record(es, None) ->
        let ee = List.map (fun (s, e) -> norm_in e) es in 
        let se = List.map (fun e -> List.hd e) ee in
        let es' = List.map2 (fun (lbl, _, loc) (s, e) -> (s, mk_ident_loc lbl loc)) se es in
        let init = mk_record es' in
        let ee = List.concat (List.rev ee) in
          rw_expr (List.fold_left (wrap Nonrecursive) init (get_back_base ee))
     | Pexp_match(e, pel) ->
        let npel = List.fold_left norm_case [] pel |> List.rev |> List.concat in
        let ls = norm_in e in
        let (lbl, _, lo) = List.hd ls in
        let init = mk_match (mk_ident_loc lbl lo) npel in
          rw_expr (List.fold_left (wrap Nonrecursive) init (get_back_base ls))
     | e -> raise (NormalizationFailure (exp, loc, "norm_out"))
(* pels is a listtype and csls is of type case *)
 and norm_case pels {pc_lhs;pc_guard;pc_rhs} =
   [(pc_lhs |> expand_or_pats |> fun p -> {pc_lhs= (List.nth p 0); pc_guard= pc_guard;pc_rhs = (norm_out pc_rhs)})] :: pels
   
  and norm_in exp = 
    let rw_expr desc = {pexp_desc = desc; pexp_loc = exp.pexp_loc; pexp_attributes = exp.pexp_attributes} in
    let proc_list es skel = 
      let this = fresh_name () in
      let lss = List.map norm_in es in
      let (lbls, lss) = List.fold_right (fun ls (ess, lss) ->
                                          let (e, ls) = resolve_in_exp_when is_const ls in
                                            (e::ess, ls::lss)) lss ([], []) in
      let e_this = Some (rw_expr (skel lbls)) in
        (this, e_this, dummy)::(List.rev (get_back_base (List.concat (lss))))
    in
    let loc = exp.pexp_loc in

    match exp.pexp_desc with
     | Pexp_assert(_)  
     | Pexp_constraint(_, _)
     | Pexp_constant(_) 
     | Pexp_construct(_, None) ->
         [(get_location (fresh_name ()), Some exp, dummy)]
     | Pexp_construct(cstrdesc, Some e) ->
         let ls = norm_in e in
         let (inex, ls) = resolve_in_exp ls in
         let this = fresh_name () in
         let e_this = Some (rw_expr (mk_construct cstrdesc inex )) in
         (get_location this, e_this, loc)::ls
     | Pexp_ident(id) ->
         [(id, None, loc)]
     | Pexp_fun(_, _, _, _)     
     | Pexp_let(_, _, _) -> 
        [(get_location (fresh_name ()), Some (norm_out exp), dummy)]
          (* pull sequences out to the closest scope *)
     | Pexp_sequence(e1,e2) ->
        let ls1 = norm_in e1 in
        let ls2 = norm_in e2 in
          List.append ls2 ls1
     | Pexp_tuple(es) ->
          get_extended ( proc_list es mk_tuple )
     | Pexp_array(es) ->
        get_extended (proc_list es mk_array )
     | Pexp_apply(e1, es) ->
        let f = norm_in e1 in
        let (flbl, e_f, lo_f) = List.hd f in
        let es = List.map (fun (_, e) -> e) es in
        let fn = match e_f with Some e -> mk_dum_ident flbl loc
                                | None -> mk_ident_loc flbl lo_f in
        let ls = proc_list es (mk_apply fn) in
        let (this, e_this, lo_this) = List.hd ls in
        (* hack for constant div *)
        let e_this =  
          if is_const_div exp || is_const_mult exp then 
            begin
            match e_this with
                Some e_this ->
                    begin match e_this.pexp_desc with
                    | Pexp_apply(e1, es') ->
                      let e_n1 = List.hd es' in
                      let e_n2 = List.nth es' 1 in
                      let e_o1 = List.hd es in
                      let e_o2 = List.nth es 1 in
                      let fst = if is_const e_o1 then (Nolabel, e_o1) else e_n1 in
                      let snd = if is_const e_o2 then (Nolabel, e_o2) else e_n2 in
                      let es' = [fst; snd] in
                      Some {pexp_desc = Pexp_apply(e1, es'); 
                            pexp_loc = e_this.pexp_loc;
                            pexp_attributes = e_this.pexp_attributes}
                    | _ -> assert false
                    end
                | None -> assert false
            end
          else e_this in 
          if is_deref e1 then norm_in (desugar_deref exp) else
          if is_assign e1 then norm_in (desugar_assign exp) else
          (get_location this, e_this, lo_this):: (get_extended (List.append (List.tl ls) (get_back_base f)))
     | Pexp_ifthenelse(e1, e2, Some e3) ->
        let (e_b, b) = resolve_in_exp_never (norm_in e1) in
        let (this, e_this, lo_this) = 
          (fresh_name (), mk_ifthenelse e_b (norm_out e2) (norm_out e3), dummy) in
         (get_location this, Some (rw_expr e_this), lo_this)::b
     | Pexp_ifthenelse(e1, e2, None) ->
         norm_in (rw_expr (Pexp_ifthenelse(e1, e2, Some un)))
     | Pexp_record(es, None) ->
        let ee = List.map (fun (s, e) -> norm_in e) es in
        let se = List.map (fun e -> List.hd e) ee in
        let es' = List.map2 (fun (lbl, _, loc) (s, e) -> (s, mk_ident_loc lbl loc)) se es in
        let e_this = rw_expr (mk_record es') in
          (get_location (fresh_name ()), Some e_this, loc)::(List.concat (List.rev ee)) 
     | Pexp_field(e, s) ->
        let ls = norm_in e in 
        let (lbl, _, lo) = List.hd ls in
          (get_location (fresh_name ()), Some (rw_expr (mk_field (mk_ident_loc lbl lo) s)), loc)::ls
     | Pexp_setfield(e, s, e') ->
        let (lhs, lls) = resolve_in_exp_never (norm_in e) in
        let (rhs, rrs) = resolve_in_exp (norm_in e') in
        let init = rw_expr (mk_setfield lhs s rhs) in
          ( get_location  (fresh_name ()), Some init, loc) :: (rrs @ lls)
     | Pexp_match(e, pel) ->
         let npel = List.fold_left norm_case [] pel |> List.rev |> List.concat in
         let ls = norm_in e in
         let (lbl, _, lo) = List.hd ls in
          (get_location( fresh_name ()), Some (rw_expr (mk_match (mk_ident_loc lbl lo) npel)), loc)::ls
     | e -> raise (NormalizationFailure (exp, loc, "norm_in"))
  in
  norm_out exp

let rec normalize_structure sstr =
 let _ = if Common.ck_olev Common.ol_normalized then Format.set_margin 170 in
  match sstr with
    [] -> []
    | {pstr_desc = (Pstr_eval (exp,[])); pstr_loc = loc} :: srem ->
        let normal_exp = normalize exp in
        let _ = Common.cprintf Common.ol_normalized "@[%a@\n@]@."  in
        ({pstr_desc = (Pstr_eval(normal_exp, [])) ; pstr_loc = loc}) :: (normalize_structure srem)
    | {pstr_desc = (Pstr_value(recursive, pl)); pstr_loc = loc} :: srem -> 
        let pl = List.map (fun {pvb_pat;pvb_expr;pvb_attributes;pvb_loc} -> 
                {pvb_pat;pvb_expr = normalize pvb_expr;pvb_attributes;pvb_loc}) pl in
        let value = {pstr_desc = (Pstr_value(recursive, pl)); pstr_loc = loc} in
        let _ = Common.cprintf Common.ol_normalized "@[%a@\n@]@."  in
          value :: (normalize_structure srem) 
    | p :: srem -> 
        p :: (normalize_structure srem) 


(* NORMALIZATION PRE-PASSES *)

(* desugar for loops *)

let desugar_for_desc wrap_loc = function
  | Pexp_for (i, s, e, u, b) ->
    let body = wrap_loc (Pexp_fun
      (Nolabel, None, { ppat_desc = i.ppat_desc; ppat_loc = Location.none; ppat_attributes = dummy_attrs }, b)) in
    let ffor = Pexp_ident (get_location (Longident.parse ((function Upto -> "Dsolve.iter_up" | Downto -> "Dsolve.iter_down") u))) |>
      wrap_loc in
    Pexp_apply (ffor, [(Nolabel, s); (Nolabel, e); (Nolabel, body)])
  | d -> d

(* let desugar_for_exp exp =
  let wrap_loc desc = { pexp_desc = desc; pexp_loc = exp.pexp_loc; pexp_attributes = exp.pexp_attributes } in
  Quotations.map_expr (desugar_for_desc wrap_loc) exp
 *)
(* let rec desugar_forloops sstr = 
  let rec des_rec = function
    | [] -> []
    | {pstr_desc = (Pstr_eval (exp,[])); pstr_loc = loc} :: srem ->
        {pstr_desc = Pstr_eval ((desugar_for_exp exp),[]); pstr_loc = loc}
        :: des_rec srem
    | {pstr_desc = Pstr_value (recursive, pl); pstr_loc = loc} :: srem ->
        let pl = List.map (fun {pvb_pat;pvb_expr;pvb_attributes;pvb_loc} -> 
                {pvb_pat;pvb_expr = (desugar_forloops pvb_expr);pvb_attributes;pvb_loc}) pl in
        {pstr_desc = Pstr_value (recursive, pl); pstr_loc = loc}
        :: des_rec srem
    | p :: srem ->
        p :: des_rec srem in
  des_rec sstr
 *)
(* eliminate Ppat_any *)

(* let elim_any_desc wrap_loc = function
  | Pexp_match (e, pel) ->
      let pel = List.map (fun (p, e) -> (elim_anys p, e)) pel in
      Pexp_match (e, pel)
  | Pexp_let (r, pes, e2) ->
      let pes = List.map (fun (p, e) -> (elim_anys p, e)) pes in
      Pexp_let (r, pes, e2)
  | Pexp_function (l, el, ps) ->
      let ps = List.map (fun (p, e) -> (elim_anys p, e)) ps in
      Pexp_function (l, el, ps)
  | d -> d

let elim_any_exp exp =
  let wrap_loc desc = { pexp_desc = desc; pexp_loc = exp.pexp_loc } in
  Quotations.map_expr (elim_any_desc wrap_loc) exp

let eliminate_anys sstr =
  let rec elim_rec = function
    | [] -> []
    | {pstr_desc = (Pstr_eval exp); pstr_loc = loc} :: srem ->
        {pstr_desc = Pstr_eval (elim_any_exp exp); pstr_loc = loc}
        :: elim_rec srem
    | {pstr_desc = Pstr_value (recursive, pl); pstr_loc = loc} :: srem ->
        let pl = List.map (fun (p, e) -> (elim_anys p, elim_any_exp e)) pl in
        {pstr_desc = Pstr_value (recursive, pl); pstr_loc = loc}
        :: elim_rec srem
    | p :: srem ->
        p :: elim_rec srem in
  elim_rec sstr
 *)