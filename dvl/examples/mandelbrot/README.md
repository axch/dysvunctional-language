Drawing the Mandelbrot Set in Javascript
========================================

This example illustrates use of the [asm.js](http://asmjs.org/)
backend for efficient client-side numerical computation.

Caution: the asm.js backend hasn't been ironed out yet; at the moment,
the output still needs to be manually post-edited.

A Haskell program for the same job, from the [Computer Language Benchmarks
Game](http://benchmarksgame.alioth.debian.org/u32/performance.php?test=mandelbrot),
is retained for comparison in `mandelbrot.ghc-2.hs`.  Unfortunately
their performance cannot be compared directly because the Haskell
benchmark uses short-circuiting, so they do not perform exactly the
same computation.

Instructions:

1.  Generate the Mandelbrot tester function from its DVL program
    with

    ```scheme
    (fol->asm.js
     (fol-optimize
      (compile-to-fol
       (dvl-source "examples/mandelbrot/mandel.dvl")))
     "examples/mandelbrot/mandel.js")
    ```

2.  Adjust the generated `__main__` function to accept two arguments
    and use them in place of the .5 and .7, respectively.  (This is a
    hack around a restriction of DVL's foreign interface that is
    incomaptible with asm.js's.)
3.  Open `mandel.html` in your favorite web browser.
4.  Fix all the asm.js syntax errors (you will need to see the browser's
    Javascript console, but turn off the browser's debugger, because at
    least in Firefox 23.0 the debugger switches asm.js off).
5.  Then click "Go!".  If you see just a gray square, make sure you got
    step 2 right (and reload the page).  If it's too slow, make sure you
    got step 4 right (and reload the page).
