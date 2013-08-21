mandelbrot.ghc-2.ghc_run: mandelbrot.ghc-2.hs
	ghc --make -O2 -XBangPatterns -rtsopts -fexcess-precision mandelbrot.ghc-2.hs -o mandelbrot.ghc-2.ghc_run

bench: mandelbrot.ghc-2.ghc_run
	time ./mandelbrot.ghc-2.ghc_run 16000 > answer.pbm

.PHONY: bench
