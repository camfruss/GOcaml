MODULES=$(addprefix src/, authors command main game graphics server util)
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=main.byte
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' ./$(TEST) && ./$(TEST)

gocaml:
	$(OCAMLBUILD) ./$(MAIN) && ./$(MAIN)

zip:
	zip gocaml.zip ./**/*.ml* ./**/*.json _tags Makefile ./**/*.md ./.merlin

clean:
	ocamlbuild -clean
	rm -rf gocaml.zip
