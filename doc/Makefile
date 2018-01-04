cinvoke.pdf: cinvoke.tex cmem.tex
	./runtex --pdf cinvoke

cinvoke.tex: cinvoke.doc
	./doc2tex $< > $@
cmem.tex: cmem.md ../prolog/cinvoke.pl
	mkdir -p summaries.d
	swipl libtotex.pl --lib=cinvoke cmem.md
