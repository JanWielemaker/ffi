cinvoke.pdf: cinvoke.tex cmem.tex clocations.tex
	./runtex --pdf cinvoke

cinvoke.tex: cinvoke.doc
	./doc2tex $< > $@
cmem.tex: cmem.md ../prolog/cinvoke.pl
	mkdir -p summaries.d
	swipl libtotex.pl --lib=cinvoke cmem.md
clocations.tex: ../prolog/clocations.pl
	mkdir -p summaries.d
	swipl libtotex.pl --outdir=. ../prolog/clocations.pl
