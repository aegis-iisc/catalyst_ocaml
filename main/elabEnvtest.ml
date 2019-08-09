



open SpecLang
module Nm = Normalize
module ElabVE = ElaborateVarEnv
module TelabVE = TestelaborateEnv
module VE = VarEnv
module RE = RelEnv
module PRE = ParamRelEnv
module VC = VerificationCondition
module VCE = Vcencode 
module TE = Testencode 


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




let call_discharge relspecs typedtree = 
      
     let ve = TelabVE.elaborate () in   

      let  _ = Printf.printf "@Var Env:\n" in 
    let  _ = Printf.printf "---------\n" in 
    
    let  _ = Printf.printf "%s" ((VE.layout ve)) in 
   

     let initial_vcs = 
      try 
      SpecVerify.doIt(ve, PRE.empty , typedtree) 
      with 
      | e -> raise e
    in    
    let  _ = Printf.printf "-- Initial vcs generation done  --------------->\n" in 
   let _ = Printf.printf "%s" (L.toString (VC.layouts initial_vcs)) in 
    
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
	    
       match Testencode.discharge pre  with
         	TE.Success -> 
            	Printf.printf  "%s" ("VC#  discharged\n")
        	| TE.Undef -> (Printf.printf "%s" ("Solver timeout  while trying to"); 
                  ();
                  (* z3_log_close (); *)
        	raise CantDischargeVC)
        	|TE.Failure -> (
        			Printf.printf "%s" ("VC is invalid!"); 
                  
                 (*  z3_log_close ();
                  *) raise CantDischargeVC)
      
    Printf.printf "%s" "is correct w.r.t given specification!\n"



 
let () = 
  let () = Printf.printf "%s" "running  z3 test" in 

  	let rel_ast = elaborateSMLWithSpec spec_file in 
	let string_ast = RelSpec.toString rel_ast in 		
	
  	let (tstr, _, _) =  get_abstract_syntax_tree in 
  	let () = Printf.printf "%s" "Print the AST" in 
  	let _ = Mytreeiter.Iterator.iter_structure  tstr in 

	  	call_discharge rel_ast tstr  
