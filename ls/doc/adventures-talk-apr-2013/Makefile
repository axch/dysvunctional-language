%.ps: %.txt
	enscript -r -M letter --highlight --color -fCourier-Bold24 -o $@ $<

celestial-out.ps: celestial-out.scm
	enscript -M letter --highlight --color -fCourier-Bold12 -U 2 --nup-xpad=0 --nup-ypad=0 -o $@ $<

%.ps: %.scm
	enscript -r -M letter --highlight --color -fCourier-Bold24 -o $@ $<

%.pdf: %.ps
	ps2pdf $< $@

presentation.pdf: mandelbrot.pdf celestial.pdf celestial-out.pdf analysis.pdf slides.pdf
	pdfjoin -o $@ mandelbrot.pdf celestial.pdf celestial-out.pdf - celestial.pdf 1 analysis.pdf slides.pdf

transparencies.pdf: mandelbrot.pdf celestial.pdf celestial-out.pdf analysis.pdf slides.pdf
	pdfjoin -o $@ mandelbrot.pdf celestial.pdf - celestial-out.pdf 1-4 analysis.pdf slides.pdf
