MODULES=$(addprefix src/, authors game command main gui util)
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=main.byte
TEST=test.byte
GUI=gui.byte
PKGS=ounit2,str,yojson,graphics,ANSITerminal
TERMINAL=y
OCAMLBUILD=ocamlbuild -use-ocamlfind

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean
	rm -rf gocaml.zip ./docs.public

docs:
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build/src -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

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
