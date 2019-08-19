#/bin/bash
ocamlbuild -use-ocamlfind -tag thread -pkg core -Is typing,parsing,utils speclang/specLang.native
