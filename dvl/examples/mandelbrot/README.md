Drawing the Mandelbrot Set in Javascript
========================================

This example illustrates use of the [asm.js](http://asmjs.org/)
backend for efficient client-side numerical computation.

Caution: the asm.js backend hasn't been ironed out yet; at the moment,
the output still needs to be manually post-edited.

Instructions:

- Generate the Mandelbrot tester function from its DVL program
  with

    ```scheme
    (fol->asm.js
     (fol-optimize
      (compile-to-fol
       (dvl-source "examples/mandelbrot/mandel.dvl")))
     "examples/mandelbrot/mandel.js")
    ```

- Adjust the `__main__` function to accept two arguments and use them
  in place of the .5 and .7, respectively.
- Open `mandel.html` in your favorite web browser.
- Fix all the asm.js syntax errors (you will need to see the browser's
  Javascript console, but turn off the browser's debugger, because at
  least in Firefox 23.0 the debugger switches asm.js off).
- Then click "Go!".  If you see just a gray square, make sure you got
  step 2 right (and reload the page).  If it's too slow, make sure you
  got step 4 right (and reload the page).
