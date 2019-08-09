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
        Format.printf "\nfound var \n";
         Format.printf "@[<2>%s@ : %a@]@."
          (Ident.name id)
         Printtyp.type_expr p.pat_type;
         (match p.pat_type.desc with
          | Tlink te ->  
            let rec tlink_to_arrow ty params =
              let td = ty.desc  in 
              match td with 
                | Tlink te' -> 
                    Format.printf " \t count  \t  ";
            
                      let params' = (te' :: params) in 
                    tlink_to_arrow te' params'  
                | _ -> (params, ty)
            in 
            let (args, res) = tlink_to_arrow te [] in      

            Format.printf " \t found Tlink  \t  ";
            Format.printf "%a" Printtyp.type_expr (p.pat_type);
            
            Format.printf " \t \n arguments  ::  ";
            let () = Printf.printf "%s" (" size   "^(string_of_int (List.length args))) in 
            
            let () = List.iter (fun arg -> Format.printf "%a" Printtyp.type_expr (arg);) args  in 
            Format.printf " \t \n res  ::  ";
            Format.printf "%a" Printtyp.type_expr (res);
            

           (* 
            (match te.desc with
                | Tlink te' ->  
                  Format.printf " \t found Tlink  \n \t ";
                Format.printf "%a" Printtyp.type_expr (te');
           

               | _ -> Format.printf "Not a Tlink##   \t ";Format.printf "%a" Printtyp.type_expr (te))  
 *)
          | _ -> Format.printf "Not a Tlink  \n ";)
    |  Tpat_construct (_, cd, pl) ->
          Format.printf "found constructor \n";
          Format.printf "%s" ("const name "^(cd.cstr_name));
          List.iter (fun e ->Format.printf "%a"
          Printtyp.type_expr e;   Format.printf "%s" ("->");) cd.cstr_args;
          Format.printf "%a" Printtyp.type_expr (cd.cstr_res);


          
   
  
    (* let enter_binding vb = 
      let pattern = vb.vb_pat in 
      let expr = vb.vb_expr in 

     *)      

       
end

module Iterator = TypedtreeIter.MakeIterator(MyIteratorArgument)


  