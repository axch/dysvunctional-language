%.ps: %.txt
	enscript -M letter --highlight --color -fCourier-Bold12 -o $@ $<

%.pdf: %.ps
	ps2pdf $< $@
