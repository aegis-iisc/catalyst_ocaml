ocamlbuild -use-menhir -tag thread -use-ocamlfind -pkg core -pkg z3 -Is typing,parsing,utils,speclang,specparser,specelab,specverify,vcencode,driver main/compile.native

