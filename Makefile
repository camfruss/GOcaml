MODULES=$(addprefix src/, authors command game go graphics player util)
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind -quiet

build:
	$(OCAMLBUILD) $(OBJECTS)
