.PHONY: all clean byte test repl utop

OCB_FLAGS = -tag bin_annot -use-menhir -use-ocamlfind -pkgs 'bindlib,oUnit'
OCB = ocamlbuild $(OCB_FLAGS)

all: byte

clean:
	$(OCB) -clean

byte:
	$(OCB) tc.byte

repl: byte
	./repl.byte

utop: byte
	utop

test:
	$(OCB) test.byte && ./test.byte
