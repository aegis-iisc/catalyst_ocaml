(* ocamlfind ocamlc -package compiler-libs.common -c example.ml *)
open Typedtree
open TypedtreeIter
open Types

module MyIteratorArgument = struct
    include DefaultIteratorArgument


                                                               
  
  let iter_case {c_lhs; c_guard; c_rhs} =
      Printf.printf "%s" "MyIter &&&&&&&&&&  Case";
                                                              

end
                                                               
  
module Iterator = TypedtreeIter.MakeIterator(MyIteratorArgument)



