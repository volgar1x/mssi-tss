OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

APPLI_OBJS=maybe.cmo dict.cmo types.cmo lexer.cmo parser.cmo eval.cmo stdlib.cmo typechecker.cmo main.cmo

appli:$(APPLI_OBJS)
	$(OCAMLC) -o appli $(APPLI_OBJS)

maybe.cmo:maybe.ml
	$(OCAMLC) -c maybe.ml

dict.cmo:dict.ml
	$(OCAMLC) -c dict.ml

eval.cmo:eval.ml
	$(OCAMLC) -c eval.ml

stdlib.cmo:stdlib.ml
	$(OCAMLC) -c stdlib.ml

types.cmo:types.ml
	$(OCAMLC) -c types.ml

typechecker.cmo:typechecker.ml
	$(OCAMLC) -c typechecker.ml

parser.ml:parser.mly
	$(OCAMLYACC) parser.mly

parser.mli:parser.mly
	$(OCAMLYACC) parser.mly

parser.cmi:parser.mli
	$(OCAMLC) -c parser.mli

parser.cmo:parser.ml
	$(OCAMLC) -c parser.ml

lexer.ml:lexer.mll
	$(OCAMLLEX) lexer.mll

lexer.cmo:lexer.ml parser.cmi
	$(OCAMLC) -c lexer.ml

main.cmo:main.ml
	$(OCAMLC) -c main.ml

clean:
	rm -f appli types.cmo lexer.cmo parser.cmo eval.cmo main.cmo types.cmi lexer.cmi parser.cmi eval.cmi main.cmi parser.mli parser.ml lexer.ml dict.cmi dict.cmo maybe.cmi maybe.cmo eval.cmi eval.cmo stdlib.cmi stdlib.cmo typechecker.cmi typechecker.cmo
