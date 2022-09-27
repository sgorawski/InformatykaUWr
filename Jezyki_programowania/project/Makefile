OBJS = lexer.cmo parser.cmo printer.cmo eval.cmo main.cmo

f: $(OBJS)
	ocamlc -o $@ $(OBJS)

include .depend

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

clean:
	rm -f f parser.mli parser.ml lexer.ml *.cmi *.cmo

depend:
	ocamldep *.mli *.ml > .depend
