open Typedtree
exception Failed of string 
let ppf = Format.err_formatter 

let env =
  Compmisc.init_path false;
  Compmisc.initial_env ()  


let ast = "let add x = x + 1" |> Lexing.from_string |> Parse.implementation   in 
let tstr, _tsig, _newe = Typemod.type_structure env ast Location.none  in 

Printtyped.implementation ppf tstr;
let str_items = match tstr with 
  | {str_items;_} -> str_items 
  | _ -> raise (Failed "failed 1") in

Printtyp.print_items env str_items None;

let str_desc = match str_items with 
  | {str_desc;_}::_ -> str_desc
  | _ -> raise (Failed "failed 2")   in 
let Typedtree.Tstr_value (_, lst) = str_desc in 

let rec helper = function
  | [] -> ()
  | vb :: rest_vb ->
      let vb_pat_i = match vb with 
        | {vb_pat;_} -> vb_pat
        |_ -> raise (Failed "failed 3") in
      match vb_pat_i with 
        {pat_type;_} -> 
          Printtyp.raw_type_expr Format.std_formatter pat_type;
          helper rest_vb in
helper lst;



