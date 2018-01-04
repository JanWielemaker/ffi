cinvoke.pdf: cinvoke.tex
	./runtex --pdf cinvoke

cinvoke.tex: cinvoke.doc
	./doc2tex $< > $@
cmem.tex: cmem.md
	mkdir -p summaries.d
	swipl libtotex.pl --lib=cinvoke cmem.md
