TARGET = main.native

all:
	ocamlbuild -use-ocamlfind -use-menhir $(TARGET)

yacc:
	ocamlbuild -use-ocamlfind $(TARGET)

clean:
	rm -rf _build $(TARGET)
