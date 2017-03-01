all: order orderedcoll prioqueue tests

order: order.ml
	ocamlbuild -use-ocamlfind order.byte

orderedcoll: orderedcoll.ml
	ocamlbuild -use-ocamlfind orderedcoll.byte

prioqueue: prioqueue.ml
	ocamlbuild -use-ocamlfind prioqueue.byte

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte

clean:
	rm -rf _build *.byte