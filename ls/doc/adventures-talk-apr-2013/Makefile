%.ps: %.txt
	enscript -r -M letter --highlight --color -fCourier-Bold16 -o $@ $<

%.pdf: %.ps
	ps2pdf $< $@
