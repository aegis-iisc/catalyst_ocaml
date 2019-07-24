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

open Types

module F = Format
module BS = Bstats

module StringMap = Map.Make(String)

module ComparablePath = struct
  type t = Path.t
  let compare = compare
  let equal = Path.same
  let hash = Hashtbl.hash
end

module PathMap = Map.Make(ComparablePath)

let qual_test_var = Path.Pident (Ident.create_persistent "AA")

let dummy_id = Ident.create_persistent ""

let incpp ir = 
  incr ir;!ir

let array_to_index_list a = 
  List.rev (snd 
    (Array.fold_left (fun (i,rv) v -> (i+1,(i,v)::rv)) (0,[]) a))

let rec pprint_many brk s f ppf = function
  | []     -> ()
  | x::[]  -> F.fprintf ppf "%a" f x
  | x::xs' -> ((if brk then F.fprintf ppf "%a %s@ " f x s else F.fprintf ppf "%a %s" f x s); 
               pprint_many brk s f ppf xs')

let pprint_list sepstr pp =
  (fun ppf -> Oprint.print_list pp
     (fun ppf -> F.fprintf ppf "%s@;<1 2>" sepstr) ppf)

let pprint_str ppf s =
  F.fprintf ppf "%s" s

let rec is_unique xs =
  match xs with
      x :: xs -> if List.mem x xs then false else is_unique xs
    | [] -> true 

let resl_opt f = function
  | Some o -> f o
  | None -> []

let resi_opt f = function
  | Some o -> f o
  | None -> ()

let opt_iter f l = 
  List.iter (resi_opt f) l

let add il i = il := i::!il
let addl il i = il := List.rev_append i !il

let same_type q p = (Types.TypeOps.equal q p)

let dummy () = Path.mk_ident ""

let same_name_id id =
  Ident.create (Ident.name id)

let same_path_i p i = Path.unique_name p = Ident.unique_name i 

let i2p i = Path.Pident i

let p2i p = match p with
    Path.Pident id -> id
  | _ -> assert false

let lookup_path s env =
  fst (Env.lookup_value (Longident.parse s) env)

let lookup_type p env =
  (Env.find_value p env).Types.val_type

let tmpstring s = if String.length s >= 2 then (s.[0] = '_' && s.[1] = '\'') || (s.[0] = 'A' && s.[1] = 'A') else false

let path_is_temp p = match Path.ident_name p with Some s -> tmpstring s | None -> false

let tuple_elem_id i =
  Ident.create ("e" ^ string_of_int i)

let abstr_elem_id () =
  Ident.create "supr" (*^ string_of_int (get_unique ()))*)


let rec maybe_list_from_singles = function
    x :: xs -> (match x with [a] -> Some a |  _ -> None) :: (maybe_list_from_singles xs)
  | [] -> []


let maybe_bool = function
  Some _ -> true
  | None -> false

let all_defined xs =
  List.for_all maybe_bool xs

  (*
let has_prefix pre s =
  try String.sub s 0 (String.length pre) = pre
    with Invalid_argument _ -> false
*)

let sub_from_list subs s =
  try List.assoc s subs with Not_found -> s

let strip_meas s =
  let start = try 1 + (String.rindex s '.') with Not_found -> 0 in
  try 
    if String.sub s start 6 = "_meas_" then 
      let pre  = String.sub s 0 start in
      let post = String.sub s (start + 6) ((String.length s) - start - 6) in
      pre ^ post
    else s
  with Invalid_argument _ -> s

(*
let sub_from s c =
  try 
    let x = String.rindex s c in
      String.sub s x ((String.length s) - x)
  with Not_found -> s

let sub_to_r s c =
  let x = String.rindex s c in
    String.sub s 0 x

let strip_meas_whole s =
  if !Clflags.dsmeasures then s else
  try 
    let start = try String.rindex s '.'
    
    if String.sub s 0 6 = "_meas_" then 
    String.sub s 6 (String.length s - 6) 
  else s with Invalid_argument _ -> s 

let rw_suff f s c =
  let suff = f (sub_from s c) in
    try (sub_to_r s c) ^ suff with Not_found -> suff

let strip_meas s =
  rw_suff strip_meas_whole s '.'
*)

let append_pref p s =
  (p ^ "." ^ s)

(*let app_fst f (a, b) = (f a, b)
let app_snd f (a, b) = (a, f b)
let app_pr f (a, b) = (f a, f b)
let app_triple f (a, b, c) = (f a, f b, f c)
*)

let l_to_s l = String.concat "." (Longident.flatten l)
let s_to_l s = Longident.parse s
let l_is_id id = function
  | Longident.Lident s -> s = id
  | _ -> false

let s_to_p s = Path.Pident (Ident.create s)

let int_of_tag = function
    Cstr_constant n -> 2*n
  | Cstr_block n -> 2*n+1
  | Cstr_extension (_,_) -> assert false
                       
let tag_of_int n = 
  if 2*(n/2) = n then
    Cstr_constant (n/2)
  else
    Cstr_block ((n-1)/2)

let sort_and_compact xs =
  let rec f = function 
    | x1::(x2::_ as xs') ->
        if x1 = x2 then (f xs') else x1::(f xs')
    | xs' -> xs' in
  f (List.sort compare xs)
 
(****************************************************************)
(************* Output levels ************************************)
(****************************************************************)

(* verbosity levels by purpose *)
let ol_always = 0
let ol_solve_error = 1
let ol_warning = 1
let ol_solve_master = 2
let ol_solve_stats = 2
let ol_timing = 2
let ol_default = 2
let ol_warn_mlqs = 3
let ol_normalized = 3
let ol_dquals = 4 
let ol_unique_names = 5 (* must be > ol_dquals *)
let ol_solve = 10 
let ol_refine = 11 
let ol_scc = 12 
let ol_dump_env = 10 
let ol_axioms = 5
let ol_dump_prover = 20
let ol_verb_constrs = 21
let ol_dump_wfs = 22
let ol_dump_meas = 30
let ol_dump_quals = 50
let ol_insane = 200

let verbose_level = ref ol_default
let verb_stack = ref []
let null_formatter = F.make_formatter (fun a b c -> ()) ignore
let nprintf a = F.fprintf null_formatter a
let ck_olev l = l <= !verbose_level

let cprintf l = if ck_olev l then F.printf else nprintf
let ecprintf l = if ck_olev l then F.eprintf else nprintf
let fcprintf ppf l = if ck_olev l then F.fprintf ppf else nprintf

let icprintf printer l ppf = if ck_olev l then printer ppf else printer null_formatter

let cprintln l s = if ck_olev l then Printf.ksprintf (F.printf "@[%s@\n@]") s else nprintf

let ident_name i = if ck_olev ol_unique_names then Ident.unique_name i else Ident.name i

let path_name p = if ck_olev ol_unique_names then Path.unique_name p else Path.name p

let elevate_olev l = if ck_olev l then () else verb_stack := !verbose_level :: !verb_stack; verbose_level := l

let restore_olev = match !verb_stack with x :: xs -> verbose_level := x; verb_stack := xs | _ -> ()

(****************************************************************)
(************* SCC Ranking **************************************)
(****************************************************************)
(*
module Int : Graph.Sig.COMPARABLE with type t = int * string =
struct
   type t = int * string 
   let compare = compare
   let hash = Hashtbl.hash
   let equal = (=)
end

module G = Graph.Imperative.Digraph.Concrete(Int)

module SCC = Graph.Components.Make(G)    

 (* Use of Graphviz *)

let io_to_string = function 
  | Some i -> string_of_int i 
  | None -> "*"

let xs_to_string f xs =
  "["^(String.concat "," (List.map f xs))^"]"

module DotGraph =
struct
   type t = G.t
   module V = G.V
   module E = G.E
   let iter_vertex = G.iter_vertex
   let iter_edges_e = G.iter_edges_e
   let graph_attributes g = []
   let default_vertex_attributes g = [`Shape `Box]
   let vertex_name (i,s) = Printf.sprintf "V_%d_%s" i s 
   let vertex_attributes v = [`Label (vertex_name v)]
   let default_edge_attributes g = []
   let edge_attributes e = []
   let get_subgraph v = None
end

module Dot = Graph.Graphviz.Dot(DotGraph) 

let dump_graph g = 
  let oc = open_out "constraints.dot" in
  Dot.output_graph oc g; 
  close_out oc


(* Given list [(u,v)] returns a numbering [(ui,ri)] s.t. 
 * 1. if ui,uj in same SCC then ri = rj
 * 2. if ui -> uj then ui >= uj *)
let scc_rank f ijs = 
  let g = G.create () in
  let _ = Bstats.time "making graph" (List.iter (fun (i,j) -> G.add_edge g (i,(f i)) (j,(f j)))) ijs in
  let _ = if !Clflags.dump_graph then dump_graph g in
  let a = SCC.scc_array g in
  let _ = cprintf ol_scc "@[dep@ graph:@ vertices@ =@ @ %d,@ sccs@ =@ %d@ @\n@]" 
          (G.nb_vertex g) (Array.length a);
          cprintf ol_scc "@[scc@ sizes:@\n@]";
          let int_s_to_string (i,s) = Printf.sprintf "(%d,%s)" i s in
          Array.iteri 
            (fun i xs -> 
               cprintf ol_scc "@[%d@ :@ %s@ @\n@]" 
                 i (xs_to_string int_s_to_string xs)) a;
          cprintf ol_scc "@[@\n@]" in
  let sccs = array_to_index_list a in
  Misc.flap (fun (i,vs) -> List.map (fun (j,_) -> (j,i)) vs) sccs

(*
let g1 = [(1,2);(2,3);(3,1);(2,4);(3,4);(4,5)];;
let g2 = [(0,1);(1,2);(2,0);(1,3);(4,3);
          (5,6);(5,7);(6,9);(7,9);(7,8);(8,5)];;
let g3 = (6,2)::g2;;
let g4 = (2,6)::g2;;
  
let n1 = make_scc_num g1 ;;
let n2 = make_scc_num g2 ;;
let n3 = make_scc_num g3 ;;
let n4 = make_scc_num g4 ;; *)


let asserts s b = 
  try assert b with ex -> 
    Printf.printf "Common.asserts failure: %s " s; raise ex

let append_to_file f s = 
  let oc = Unix.openfile f [Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT] 420  in
  ignore (Unix.write oc s 0 ((String.length s)-1) ); 
  Unix.close oc

let write_to_file f s =
  let oc = open_out f in
  output_string oc s; 
  close_out oc
*)
(**************************************************************************)
(****************** Type Specific to_string routines **********************)
(**************************************************************************)

let fsprintf f p = 
  F.fprintf F.str_formatter "@[%a@]" f p;
  F.flush_str_formatter ()
(*
let pred_to_string p = 
  fsprintf Predicate.pprint p
*)

(*************************************************************************)

let map_cnt f m =
  let cnt a b n = n + 1 in
    f cnt m 0

let set_cnt f s =
   List.length (f s)

(******************************************************************************)
(********************************* Fting *********************************)
(******************************************************************************)

let space ppf =
  F.fprintf ppf "@;<1 0>"

let rec same_length l1 l2 = match l1, l2 with
  | [], []           -> true
  | _ :: xs, _ :: ys -> same_length xs ys
  | _                -> false

(******************************************************************************)
(********************************* Mem Management *****************************)
(******************************************************************************)

open Gc
(* open Format *)

let pprint_gc s =
  (*printf "@[Gc@ Stats:@]@.";
  printf "@[minor@ words:@ %f@]@." s.minor_words;
  printf "@[promoted@ words:@ %f@]@." s.promoted_words;
  printf "@[major@ words:@ %f@]@." s.major_words;*)
  (*printf "@[total allocated:@ %fMB@]@." (floor ((s.major_words +. s.minor_words -. s.promoted_words) *. (4.0) /. (1024.0 *. 1024.0)));*)

  F.printf "@[total allocated:@ %fMB@]@." (floor ((allocated_bytes ()) /. (1024.0 *. 1024.0)));
  F.printf "@[minor@ collections:@ %i@]@." s.minor_collections;
  F.printf "@[major@ collections:@ %i@]@." s.major_collections;
  F.printf "@[heap@ size:@ %iMB@]@." (s.heap_words * 4 / (1024 * 1024));
  (*printf "@[heap@ chunks:@ %i@]@." s.heap_chunks;
  (*printf "@[live@ words:@ %i@]@." s.live_words;
  printf "@[live@ blocks:@ %i@]@." s.live_blocks;
  printf "@[free@ words:@ %i@]@." s.free_words;
  printf "@[free@ blocks:@ %i@]@." s.free_blocks;
  printf "@[largest@ free:@ %i@]@." s.largest_free;
  printf "@[fragments:@ %i@]@." s.fragments;*)*)
  F.printf "@[compactions:@ %i@]@." s.compactions;
  (*printf "@[top@ heap@ words:@ %i@]@." s.top_heap_words*) ()

let dump_gc s =
  F.printf "@[%s@]@." s;
  pprint_gc (Gc.quick_stat ())

(* ************************************************************* *)
(* ************************ ml_types *************************** *)
(* ************************************************************* *)

let rec copy_type = function
  | {desc = Tlink t} -> copy_type t (* Ensures copied types gets target's id/level, not link's *)
  | t                -> {t with desc = Btype.copy_type_desc copy_type t.desc}

(* ************************************************************* *)
(* ************************ core_types ************************* *)
(* ************************************************************* *)
(*open Parsetree

let map_core_type_constrs f t = 
  let rec map_rec t =
    let wrap a = {ptyp_desc = a; ptyp_loc = t.ptyp_loc} in
    match t.ptyp_desc with
    | Ptyp_arrow (l, t, t') -> wrap (Ptyp_arrow (l, map_rec t, map_rec t'))
    | Ptyp_tuple ts -> wrap (Ptyp_tuple (List.map map_rec ts))
    | Ptyp_constr (l, ts) -> wrap (Ptyp_constr (f l, List.map map_rec ts))
    | Ptyp_alias (t, s) -> wrap (Ptyp_alias (map_rec t, s))
    | t -> wrap t in
  map_rec t

let rec prover_t_to_s = function
    | Pprover_abs s -> s
    | Pprover_array (s, t) -> "[ " ^ (prover_t_to_s s) ^ "; " ^ (prover_t_to_s t) ^ " ]"
    | Pprover_fun ss -> "( " ^ (String.concat " -> " (List.map prover_t_to_s ss)) ^ " )"
  *)
