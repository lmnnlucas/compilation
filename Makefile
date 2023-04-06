all: parser_explanations interpreter.out visualiser.out doc.html

interpreter.out: $(wildcard */*.ml */*.mli */*.mly */*.mll)
	rm -rf interpreter.out
	dune build
	ln -s _build/default/bin/main.exe interpreter.out

visualiser.out: $(wildcard */*.ml */*.mli *.*/mly */*.mll)
	rm -rf visualiser.out
	dune build
	ln -s _build/default/bin/visualiser.exe visualiser.out

doc.html:
	dune build @doc
	ln -s _build/default/_doc/_html/index.html doc.html

.PHONY: parser_explanations
parser_explanations: $(parser/Parser.mly)
	rm -rf _build visualiser.out interpreter.out doc.html
	dune build
	cp _build/default/parser/Parser.dot _build/default/parser/Parser.conflicts _build/default/parser/Parser.automaton .

.PHONY: clean
clean:
	rm -rf doc.html interpreter.out Parser.automaton Parser.conflicts Parser.dot visualiser.out
	rm -rf _build