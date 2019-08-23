OCaml Implementation for Catalyst[1]
A relational Specification framework as a dependent typesystem for OCaml called OCatalyst

Running
==========
./ocatalyst.native <test/path_to_ml_file> <test/path_to_spec_file>


To Compile 
===========

We are currently building  OCatalyst using "ocamlbuild" tool.

Dependencies
-------------
1.The OCaml compiler, version 4.03.0
2. menhir, version 20180905 for Parsing and front-end design 
3. Ocaml "core" pkg 
4. Z3 libraries for ML


Compiling with ocamlbuild 
-------------------------
cd OCatalyst_home 

ocamlbuild -use-menhir -tag thread -use-ocamlfind -pkg core -pkg z3 -Is typing,parsing,utils,speclang,specparser,specelab,specverify,vcencode,driver main/ocatalyst.(native|byte)



Current Status 
===============
Experiments
-----------
1. Monomorphic lists with all major OCaml list library functions-  
    - cons, concat, rev, append, revappend, identity, tail. 
2. A simplified version of the fold and map with given shape specifications for higher order functions. 
    - We get the correctness of the fold and map implementations for free for the given shape specifications.
    - The mapincorrect.spec fails to typecheck as expected.
3. These tests are stored in "OCatalyst_home/test"


Ongoing Extensions in order of priority (timeline- approximate estimated date of completion)
---------------------------------------
1. The implementation currently does not handle user-defined datatypes (inductive and simple). This will be supported next. (timeline- 08/26)
	- Handling inductive types like Trees, Binary Trees, etc. will allow us to check more interesting relational properties. This will be supported along with the related experimentations. 
2. The current implementation requires the programmer to provide an A-normal form implementation for the test inputs and has a limited automatic A-normal form translation. 
    We plan to have a more complete A-normal form translation for a good subset of Ocaml programs. (timeline 09/02)	
3. Inferring variable types from the user-provided specifications.
    -The current implementation uses the type inference of OCaml to get the base types of variables, this is efficient but imprecise in certain cases, We plan to aid this with the base types provided by the programmer as a part of the definitions for the relations. (timeline 09/02)  

4. Handling Parametric Relations and Parametric dependent types and experimentations. (timeline 09/12)
5. The current implementation, only allows refinement relations like (R1 = R2, R1 C R2 and R1 C= R2), 
	we plan to extend these with relations invoving implication and double implication. (09/15)
6. A better pretty-printing library for printing the final and intermediate results. (09/15)

	



Refernces:
[1] Gowtham Kaki and Suresh Jagannathan. 2014. A relational framework for higher-order shape analysis. In Proceedings of the 19th ACM SIGPLAN international conference on Functional programming (ICFP '14). ACM, New York, NY, USA, 311-324. DOI: https://doi.org/10.1145/2628136.2628159
