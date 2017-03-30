.PHONY: run build clean

build:
	ocamlbuild -use-ocamlfind 'main.native'

run:
	make build && ./main.native; make clean

clean:
	rm -rf _build
