open SpecLang
module Nm = Normalize
module ElabVE = ElaborateVarEnv
module VE = VarEnv
module RE = RelEnv
module PRE = ParamRelEnv
module VC = VerificationCondition
module VCE = Vcencode 
 
exception CompilerEx of string
exception CantDischargeVC
let ppf = Format.err_formatter 

let elaborateSMLWithSpec  =
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
  

let get_abstract_syntax_tree ml_file  = 
  let () = Printf.printf "%s" ("\n\n ************************************") in 
  let () = Printf.printf "%s" ("\n\n Building AST for the ml program") in 
  let () = Printf.printf "%s" ("\n\n ************************************") in
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
  let () = Printf.printf "%s" ("\n\n ************************************") in
  let () = Printf.printf "%s" ("\n\n Building Typedtree for AST program") in 
  let () = Printf.printf "%s" ("\n\n ************************************") in
  let (tstr, _tsig, _newe) = 
    try 
    Typemod.type_structure env ast Location.none  
  with 
  | _ -> raise (CompilerEx "The typedtree structure cannot be created for the given ast, look for some missing function definitions")
  in
  (tstr, _tsig, _newe)


let catalyst_elaborate_envs relspecs typedtree = 
    
    let speclang = relspecs in 
    let () = Printf.printf "%s" ("\n\n **************************************************") in
    let () = Printf.printf "%s" ("\n\n Buildiing Initial VarEv, RE and PRE using Typedtree and Specifications") in 
    let () = Printf.printf "%s" ("\n\n ***************************************************") in
  
    let (ve, re, pre) =  ElabVE.elaborate typedtree speclang in 
    let  _ = Printf.printf "\n Initial Var Env:\n" in 
    let  _ = Printf.printf "---------\n" in 
      
    let  _ = Printf.printf "%s" ((VE.layout ve)) in 
    let  _ = Printf.printf "\n Initial Rel Env:\n" in
    let  _ = Printf.printf "-----------\n" in 
       
    let _ = Printf.printf "%s" (L.toString (RE.layout re)) in 
    let _ = Printf.printf "\n Initial Param Rel Env:\n" in
    let  _ = Printf.printf "--------------\n" in 
       
    let _ = Printf.printf "%s" (L.toString (PRE.layout pre)) in 
    let _ = Printf.printf  "\n" in 
           

     (*get the  verification conditions*)
   

    let () = Printf.printf "%s" ("\n\n **********************************************************************") in
    let () = Printf.printf "%s" ("\n\n Buildiing Initial VC using the VE and typedtree and Typechecking Rules") in 
    let () = Printf.printf "%s" ("\n\n ***********************************************************************") in
  
    let initial_vcs = 
      try 
      SpecVerify.doIt(ve, PRE.empty , typedtree) 
      with 
      | e -> raise e
    in    
  
    let () = Printf.printf "%s" ("\n\n **************************************************") in
    let () = Printf.printf "%s" ("\n\n Expanding the VCs using the VE and typedtree**********") in 
    let () = Printf.printf "%s" ("\n\n ***************************************************") in
      
    let elaborated_vcs = 
        List.map (fun vc -> VC.elaborate (re,pre,vc)) initial_vcs in 
           
      let _ = Printf.printf "%s" ("\n Elaborated VCS \n") in 
      let _ = Printf.printf "%s" (string_of_int (List.length elaborated_vcs)) in 
      let _ = Printf.printf  "\n" in 

     let _ = Printf.printf "%s" (L.toString (VC.layouts elaborated_vcs)) in 
      

    let () = Printf.printf "%s" ("\n\n **************************************************") in
    let () = Printf.printf "%s" ("\n\n Discharging the VCs to Z3**********") in 
    let () = Printf.printf "%s" ("\n\n ***************************************************") in
  
      let  dischargeVC i vc = 
      let _ = Printf.printf "%s" ("Discharging VCS "^(string_of_int (i+1))^" of "^(string_of_int (List.length elaborated_vcs))^"\n") in 
          
          (*    if ( (* i = 0 || i = 1 || i= 2 || i=3 || i=4 || i=5|| *)i=6 || i=7 ||  i = 8  ) then Printf.printf "Skipping VC1 " 
           else 
    *)
          match VCE.discharge vc with
          VCE.Success -> 
            Printf.printf  "%s" ("VC# "^(string_of_int (i+1))^" discharged\n")
        | VCE.Undef -> (Printf.printf "%s" ("Solver timeout  while trying to \
                  \discharge VC #"^(string_of_int (i+1))); 
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
      
    Printf.printf "%s" "The implementation is correct w.r.t given specification!\n"
    
 
let () = 
  
  let num_args = (Array.length Sys.argv) - 1 in 
  if num_args = 2 
  then  
    let ml_file = Sys.argv.(1) in 
    let spec_file = Sys.argv.(2) in 

    let _ = Printf.printf "%s" ("\nmlfile :: "^ml_file) in 

    let _ = Printf.printf "%s" ("\nspecfile :: "^spec_file) in 
        

    	let rel_ast = elaborateSMLWithSpec spec_file in 
    	let string_ast = RelSpec.toString rel_ast in 		
    		Printf.printf "%s" string_ast;

      let (tstr, _, _) =  get_abstract_syntax_tree ml_file in 
      (* let () = Printf.printf "%s" "Print the AST" in 
      let _ = Mytreeiter.Iterator.iter_structure  tstr in 
   *)
      catalyst_elaborate_envs rel_ast tstr; 
   
  else
    let exception_msg = (" Incorrect Number of argumnets, required 2, provided "^(string_of_int num_args)^" Usage :: ./compile.native <path_to_ml_file> <path_to_spec_file>") in  
    raise (CompilerEx  exception_msg)
 
  
