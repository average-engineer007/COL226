.PHONY: run
run:
	ocamlc -c ast.ml
	ocamllex lexer.mll       
	ocamlyacc parser.mly     
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml 
	ocamlc -o main lexer.cmo parser.cmo ast.cmo main.cmo 
	cat test.txt | ./main
clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli main