
MLI := $(wildcard src/*.mli)
ML  := $(MLI:.mli=.ml)


all: main

main: main.native

main.native:  $(MLI) $(ML)
	ocamlbuild src/main.native

clean:
	ocamlbuild -clean

.PHONY: main main.native
