MODULES=$(addprefix src/, authors command main game gui server util)
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=main.byte
TEST=test.byte
GUI=gui.byte
TERMINAL=y

OCAMLBUILD=ocamlbuild -use-ocamlfind

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean
	rm -rf gocaml.zip

gocaml:
	if [ "$(TERMINAL)" = "y" ]; \
	  then $(OCAMLBUILD) ./$(MAIN) && ./$(MAIN); \
  else \
	  $(OCAMLBUILD) ./$(GUI) && ./$(GUI); \
  fi

test:
	$(OCAMLBUILD) -tag 'debug' ./$(TEST) && ./$(TEST)

zip:
	zip gocaml.zip ./games/* ./src/* ./tests/* ./tests/supporting/* _tags Makefile ./*.md ./.merlin
