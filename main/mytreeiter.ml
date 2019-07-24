(* ocamlfind ocamlc -package compiler-libs.common -c example.ml *)
open Typedtree
open TypedtreeIter
open Types

module MyIteratorArgument = struct
  include DefaultIteratorArgument


  let enter_core_type ct = 
    let () = Format.printf "core-type" in
    match ct.ctyp_desc with 
    |Ttyp_constr (_,loc, ctlist)->
        Format.printf "found core-typ";
        
        Format.printf "@[<2>%s@ : %a@]@."
          ("location info")
          Printtyp.longident (loc.txt)





  let enter_pattern p = match p.pat_desc with
    | Tpat_var (id, _) ->
        Format.printf "found var \n";
        
        Format.printf "@[<2>%s@ : %a@]@."
          (Ident.name id)
          Printtyp.type_expr p.pat_type;
    |  Tpat_construct (_, cd, pl) ->
          Format.printf "found constructor \n";
        
          (* List.iter (fun te -> Printtyp.type_expr te) (cd.cstr_args);
 *)
  
    (* let enter_binding vb = 
      let pattern = vb.vb_pat in 
      let expr = vb.vb_expr in 

     *)      

       
end

module Iterator = TypedtreeIter.MakeIterator(MyIteratorArgument)


  