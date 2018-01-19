ffi.pdf: ffi.tex cmem.tex clocations.tex
	./runtex --pdf ffi

ffi.tex: ffi.doc
	./doc2tex $< > $@
cmem.tex: cmem.md ../prolog/ffi.pl
	mkdir -p summaries.d
	swipl libtotex.pl --lib=ffi cmem.md
clocations.tex: ../prolog/clocations.pl
	mkdir -p summaries.d
	swipl libtotex.pl --outdir=. ../prolog/clocations.pl

clean:
	rm -f *~
	rm -f ffi.aux ffi.idx ffi.ilg ffi.log ffi.out ffi.tex tti.toc
	rm -f cmem.tex clocations.tex

distclean: clean
	rm -f ffi.pdf
