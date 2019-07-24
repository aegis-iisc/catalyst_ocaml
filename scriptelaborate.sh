#/bin/sh
ocamlbuild -use-ocamlfind -pkg core -Is typing,parsing,utils,speclang,specelab  specelab/elaborateVarEnv.byte
