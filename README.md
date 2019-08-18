OCaml Implementation for Catalyst[1]
A relational Specification framework as a dependent typesystem for OCaml.

Running
==========
./OCatalyst file.ml file.spec


To Compile 
===========

We are currently building  OCatalyst using "ocamlbuild" tool.

Dependencies
-------------
1.The OCaml compiler, version 4.03.0
2. menhir, version 20180905 for Parsing and front-end design 
3. Ocaml "core" pkg 
4. Z3 libraries for ML


Compiling 
-------------
cd OCatalyst_home
ocamlbuild -use-menhir -tag thread -use-ocamlfind -pkg core -pkg z3 -Is typing,parsing,utils,speclang,specparser,specelab,specverify,vcencode,driver main/compile.native


Things Handled
1. Monomorphic lists with all major OCaml list library functions like concat, rev, append, revappend, cons, tail,  etc.
2. Simple fold_left and fold_right, with trivial specifications for higher order functions. (This will extended with polymorphic higher order functions in the next update.)
	- We get the correctness proof for the trivial case for free.
3. Checked the incorrectness of a simple version of the map.


Things in the next update
1. Handling User defined datatypes.
2. Handle inductive datatypes-
	Handling, inductive types like a Tree will allow us to check relational properties over them. 
2. Inferring variable sorts from the user provided specifications.
	The current implementation uses the type inference of OCaml to get the base types of variables, this can be aided with the base types provided by the programmer and the definitions for the relations.  
3. Extending Anormal form for extended OCaml
4. Handling Parametric Relations and Parametric dependent types.



