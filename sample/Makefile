OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

OBJ = jlite_structs.cmo parser.cmo lexer.cmo main.cmo

all: $(OBJ)
	ocamlc -o Jlite $^

clean:
	rm *.cmo *.cmi *.output

.SUFFIXES: .ml .cmo .mll .mly .mli 

.ml.cmo:
	$(OCAMLC) -c $(BFLAGS) $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.mli:
	$(OCAMLYACC) -v $<

.mli.ml:
	$(OCAMLC) -c $<
