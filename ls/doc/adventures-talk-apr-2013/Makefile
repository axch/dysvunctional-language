%.ps: %.txt
	enscript -r -M letter --highlight --color -fCourier-Bold24 -o $@ $<

%.pdf: %.ps
	ps2pdf $< $@
