(*-----------------------------------------*)
(*			Lex And Parse Spec			   *)
(*-----------------------------------------*)
(* We need to do the following 
 * expand elaborate_sml_with spec  for ML 
 * create a main file called catalyst 
 * compile the front end code with this as the target 
 * exceute*)





open SpecLang
module Nm = Normalize
module ElabVE = ElaborateVarEnv
module VE = VarEnv
module RE = RelEnv
module PRE = ParamRelEnv
module VC = VerificationCondition
module VCE = Vcencode 


let spec_file = "concat.spec"
let ml_file = "concat.ml"

exception CompilerEx of string
exception CantDischargeVC
let ppf = Format.err_formatter 

let elaborateSMLWithSpec =
      fun spec -> 
        (*work on just one file*)
         try  
        let specast = SpecFrontEnd.lex_and_parse_file (spec)
          in specast
         with 
        | e -> raise e

let env = 
  try 
  Compmisc.init_path false;
  Compmisc.initial_env ()   
  with 
  | e -> raise e
  

let get_abstract_syntax_tree  = 
 let ast = 
  SpecFrontEnd.load_file ml_file  
  |> Lexing.from_string 
  |> Parse.implementation  
  (* |> try 
      Nm.normalize_structure 
      with Nm.NormalizationFailure (e, t, m) ->
      Format.printf "@[Normalization failed "
     ; assert false 
   *)in
                    
  let (tstr, _tsig, _newe) = 
    try 
    Typemod.type_structure env ast Location.none  
  with 
  | _ -> raise (CompilerEx "TypeMod failed")
  in
  (tstr, _tsig, _newe)


let catalyst_elaborate_envs relspecs typedtree = 
(*The logic of elaborateSMLWithSpec goes here*)
   (*  let norm_sourcefile x =
            try Nm.normalize_structure typedtree 
         with Nm.NormalizationFailure (e, t, m) ->
      (Format.printf "@[Normalization failed at %a(%s) %a@.@]") ;false;
        in 
    let a_norm_tree = norm_sourcefile typedtree in 
    *) 
    
    let speclang = relspecs in 
    let (ve, re, pre) =  ElabVE.elaborate typedtree speclang in 
    let  _ = Printf.printf "@Var Env:\n" in 
    let  _ = Printf.printf "---------\n" in 
    
    let  _ = Printf.printf "%s" ((VE.layout ve)) in 
    let  _ = Printf.printf "\n@Rel Env:\n" in
    let  _ = Printf.printf "-----------\n" in 
     
    let _ = Printf.printf "%s" (L.toString (RE.layout re)) in 
    let _ = Printf.printf "\n@Param Rel Env:\n" in
    let  _ = Printf.printf "--------------\n" in 
     
    let _ = Printf.printf "%s" (L.toString (PRE.layout pre)) in 
     let _ = Printf.printf  "\n" in 
           

     (*get the  verification conditions*)

    let initial_vcs = 
      try 
      SpecVerify.doIt(ve, PRE.empty , typedtree) 
      with 
      | e -> raise e
    in    
    let elaborated_vcs = 
        List.map (fun vc -> VC.elaborate (re,pre,vc)) initial_vcs in 
      let  _ = Printf.printf "----------\n" in 
    
      let _ = Printf.printf "%s" ("Elaborated VCS") in 
      let _ = Printf.printf "%s" (L.toString (VC.layout elaborated_vcs)) in 
     let _ = Printf.printf  "\n" in 
    

        let  dischargeVC i vc = 
          match VCE.discharge vc with
          VCE.Success -> 
            Printf.printf  "%s" ("VC# "^(string_of_int i)^" discharged\n")
        | VCE.Undef -> (Printf.printf "%s" ("Solver timeout  while trying to \
                  \discharge VC #"^(string_of_int i)); 
                  ();
                  (* z3_log_close (); *)
        raise CantDischargeVC)
        |VCE.Failure -> (Printf.printf "%s" ("VC # " ^(string_of_int i)^
                " is invalid!"); 
                  ();
                 (*  z3_log_close ();
                  *) raise CantDischargeVC)
    in  

    let unit_lists = List.mapi dischargeVC elaborated_vcs in   

   (*  let _ = z3_log_close () in 
    *)
      
    Printf.printf "%s" "is correct w.r.t given specification!\n"



 
let () = 
  let () = Printf.printf "%s" "HI" in 
	let rel_ast = elaborateSMLWithSpec spec_file in 
	let string_ast = RelSpec.toString rel_ast in 		
		Printf.printf "%s" string_ast;

  let (tstr, _, _) =  get_abstract_syntax_tree in 
  let () = Printf.printf "%s" "Print the AST" in 
  let _ = Mytreeiter.Iterator.iter_structure  tstr in 

  catalyst_elaborate_envs rel_ast tstr; 



       	  
(* 
let catalyst_parse_spec_file (ppf:Format) (fileName:string) : SL.RelSpec.T =
  let elaborateSMLWithSpec =
      fun specfile -> 
         try  
          let specast = SpecFrontEnd.lex_and_parse_file (specfile)
          in specast
         with 
        | e -> raise e
  in       
  let relspec_ast = elaborateSMLWithSpec file in 
  let string_ast = RelSpec.toString relspec_ast in    
    
     fprintf ppf "%a@" string_ast;
    relspec_ast

 *)
