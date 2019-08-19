#/bin/sh
ocamlbuild -use-ocamlfind -pkg core -Is typing,parsing,utils,speclang,specelab  specverify/verificationCondition.byte

