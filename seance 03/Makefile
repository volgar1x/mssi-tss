.PHONY: run build clean

TARGET=appli
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

APPLI_OBJS=maybe.cmo dict.cmo types.cmo parser.cmo lexer.cmo typechecker.cmo eval.cmo stdlib.cmo main.cmo

$(TARGET): build

parser.cmi: parser.mli
parser.cmo: parser.ml parser.cmi
parser.ml: parser.mly
	$(OCAMLYACC) $<

lexer.ml: lexer.mll
	$(OCAMLLEX) $<

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

run:
	make build && ./$(TARGET); make clean

build:$(APPLI_OBJS)
	$(OCAMLC) -o $(TARGET) $(APPLI_OBJS)

clean:
	rm -f $(TARGET) \
		$(APPLI_OBJS) \
		$(APPLI_OBJS:.cmo=.cmi) \
		parser.ml parser.mli \
		lexer.ml
