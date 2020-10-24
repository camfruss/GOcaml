MODULES=$(addprefix src/, authors command main game gui server util)
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=main.byte
TEST=test.byte
GUI=gui.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean
	rm -rf gocaml.zip

gocaml:
	$(OCAMLBUILD) ./$(MAIN) && ./$(MAIN)

gui:
	$(OCAMLBUILD) ./$(GUI) && ./$(GUI)

test:
	$(OCAMLBUILD) -tag 'debug' ./$(TEST) && ./$(TEST)

zip:
	zip gocaml.zip ./**/*.ml* ./**/*.json _tags Makefile ./**/*.md ./.merlin
