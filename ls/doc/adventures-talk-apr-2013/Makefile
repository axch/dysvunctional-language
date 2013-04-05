%.ps: %.txt Makefile
	enscript -r -M letter --highlight --color -fCourier-Bold24 -o $@ $<

celestial-out.ps: celestial-out.scm Makefile
	enscript -M letter --highlight --color -fCourier-Bold12 -U 2 --nup-xpad=0 --nup-ypad=0 -o $@ $<

%.ps: %.scm Makefile
	enscript -r -M letter --highlight --color -fCourier-Bold24 -o $@ $<

%.pdf: %.ps
	ps2pdf $< $@
