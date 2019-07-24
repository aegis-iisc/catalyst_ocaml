(* ocamlfind ocamlc -package compiler-libs.common -c example.ml *)
open Typedtree
open TypedtreeIter

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
    | _ -> ()

let enter_expression exp = 

        match exp.exp_desc with 
        | Texp_constant c -> 
            Format.printf "found constant exp \n";
            Format.printf "@[<2>%s@ : %a@]@."
            ("constant"
            )
            Printtyp.type_expr exp.exp_type;
        | _ -> ()     


let enter_pattern p = match p.pat_desc with
    | Tpat_var (id, _) ->
        Format.printf "found var \n";
        
        Format.printf "@[<2>%s@ : %a@]@."
          (Ident.name id)
          Printtyp.type_expr p.pat_type;

        let ptype =  p.pat_type in 
        let desc = ptype.desc in 
        
        match desc with 
        | Tvar (Some s) -> Format.printf "%s" ("Tvar"^s)
                (** [Tvar (Some "a")] ==> ['a] or ['_a]
                 *       [Tvar None]       ==> [_] *)


        | Tconstr (_ , te_list, _) -> Format.printf "%s" "Tconstr" 

                     (** [Tconstr (`A.B.t', [t1;...;tn], _)] ==> [(t1,...,tn) A.B.t]
                      *       The last parameter keep tracks of known expansions, see [abbrev_memo]. *)

         | Tarrow (_,_,_,_) -> Format.printf "Tarrow \n"
        
         | Ttuple (_) -> Format.printf "TTuple \n"
                 
         | Tfield (_,_,_,_) -> Format.printf "Tfield \n"
         | Tnil -> Format.printf "Tnil \n"
                 

        | Tobject (te, _) -> Format.printf "TObj \n" (*type_expr * (Path.t * type_expr list) option ref*)

                      
        | _ -> Format.printf "Some other \n"  ;

        Format.printf "\n _______________________\n";    
        (*Format.printf "@[<2>%s@ : %a@]@."  
          (Ident.name id)
          Location.print_loc p.pat_loc*)
    | _ -> ()
end

module Iterator = TypedtreeIter.MakeIterator(MyIteratorArgument)



exception Failed of string 

let ppf = Format.err_formatter 

let env =
  Compmisc.init_path false;
  Compmisc.initial_env ()  

let () = 

  let ast = "let x = true
                        let add x = x + 1 
              let foo = fun x -> x + 1" |> Lexing.from_string |> Parse.implementation  in
  let tstr, _tsig, _newe = Typemod.type_structure env ast Location.none  in 
  Iterator.iter_structure  tstr;

