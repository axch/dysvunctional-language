Predicting the Motions of the Planets with Modularity
=====================================================

This is the most elaborate DVL exmaple.  What we have here is a
program to integrate the motions of the four Jovian planets around the
Sun.  There isn't much here in the way of fancy numerical
methods---just the Runge-Kutta family of integrators (RK4 by
default)---but you should see how clear and modular this program is.
A couple quotations from the code:

- "The forces are minus the gradient of the potential energy at the positions"
  (that gradient is computed by automatic differentiation of course,
  not by numerical approximation)

    ```scheme
    (define (forces objects)
      (* -1 (gradient (positions->potential objects) (map position objects))))
    ```

- "The derivatives of the masses, positions, and velocities are
  0, the velocities, and the forces, respectively"

    ```scheme
    (map3 make-object
          (map (lambda (obj) 0) objects)
          (map velocity objects)
          (map2 / (forces objects) (map mass objects))))))
    ```

- "We want the stream of samples of the integrator applied to the
  state derivative at the initial conditions"

    ```scheme
    (step-stream rk4-again state-derivative initial-conditions)
    ```

Try writing this program like this in Fortran and see what happens!
And yet, DVL compiles this to as ugly and fast a mess as you could
wish for.  If you want to try it out, read the instructions in
`celestial-driver.scm`.  There is also a driver for compiling to SBCL
in `celestial-driver.lisp`, but it doesn't draw the diagnostic graphic
as it goes.
