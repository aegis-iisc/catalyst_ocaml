#/bin/bash
ocamlbuild -use-menhir -tag thread -use-ocamlfind -pkg core -Is typing,parsing,utils,speclang specparser/specParser.native
