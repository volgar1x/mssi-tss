.PHONY: run build clean

build:
	ocamlbuild -use-ocamlfind -pkgs 'batteries' 'main.native'

run:
	make build && ./main.native; make clean

clean:
	rm -rf _build 'main.native'
