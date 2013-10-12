;;; ----------------------------------------------------------------------
;;; Copyright 2010-2011 National University of Ireland.
;;; ----------------------------------------------------------------------
;;; This file is part of DysVunctional Language.
;;; 
;;; DysVunctional Language is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;;  License, or (at your option) any later version.
;;; 
;;; DysVunctional Language is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Affero General Public License
;;; along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

(declare (usual-integrations))
(in-test-group
 cse

 (define-each-check
   ;; Eliminate common (+ 2 x)
   (equal?
    '(let ((x (real 5)))
       (let ((y (+ 2 x)))
         (+ y y)))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((y (+ 2 x)))
          (let ((z (+ 2 x)))
            (+ y z))))))

   ;; Eliminate common (+ 2 x) in parallel let
   (equal?
    '(let ((x (real 5)))
       (let ((y (+ 2 x)))
         (+ y y)))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((y (+ 2 x)) (z (+ 2 x)))
          (+ y z)))))

   ;; See through (+ x 0)
   (equal?
    '(let ((x (real 5)))
       (+ x x))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((y (+ x 0)))
          (let ((z (+ x 0)))
            (+ y z))))))

   ;; Simplify (+ x 0)
   (equal? '(let ((x (real 4))) x)
           (intraprocedural-cse '(let ((x (real 4))) (+ x 0))))

   ;; Even when the "x" is something hairy
   (equal? '(let ((x (real 4))) x)
           (intraprocedural-cse
            '(+ (let ((x (real 4))) x) 0)))

   ;; TODO Abstract the test pattern "CSE didn't eliminate this thing
   ;; it shouldn't have eliminated"?

   ;; Leave non-common things be
   (equal?
    '(let ((x (real 5)))
       (let ((y (+ 3 x)))
         y))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((y (+ 3 x)))
          y))))

   ;; Respect variable scope
   (equal?
    '(let ((x (real 5)))
       (let ((w (let ((y (+ 3 x))) y)))
         w))
    (%intraprocedural-cse
     '(let ((x (real 5)))
        (let ((w (let ((y (+ 3 x))) y)))
          w))))

   ;; Continue respecting variable scope
   (equal?
    '(let ((x (real 5)))
       (let ((w (let ((y (+ 3 x))) y)))
         (let ((z (+ 3 x)))
           (+ w z))))
    (%intraprocedural-cse
     '(let ((x (real 5)))
        (let ((w (let ((y (+ 3 x))) y)))
          (let ((z (+ 3 x)))
            (+ w z))))))

   ;; But don't let variable scope interfere with finding other common
   ;; expressions
   (equal?
    '(let ((x (real 5)))
       (let ((w (let ((y (+ 3 x))) y)))
         (let ((z (+ 3 x)))
           (+ w (* z z)))))
    (%intraprocedural-cse
     '(let ((x (real 5)))
        (let ((w (let ((y (+ 3 x))) y)))
          (let ((z (+ 3 x)))
            (let ((u (+ 3 x)))
              (+ w (* z u))))))))

   ;; This CSE is intraprocedural
   (equal?
    '(begin
       (define (op a b)
         (argument-types real real (values real real))
         (values (+ a b) (- a b)))
       (let-values (((x y) (op 1 2)))
         (+ x y)))
    (%intraprocedural-cse
     '(begin
        (define (op a b)
          (argument-types real real (values real real))
          (values (+ a b) (- a b)))
        (let-values (((x y) (op 1 2)))
          (+ x y)))))

   ;; I know that x+1 is (+ x 1) in both branches of the IF
   (equal?
    '(let ((x (real 3)))
       (let-values
           (((x+1 something)
             (if (> (real 2) 1)
                 (values (+ 1 x) (+ 4 x))
                 (values (+ 1 x) (+ 3 x)))))
         (* something (+ x+1 x+1))))
    (%intraprocedural-cse
     '(let ((x (real 3)))
        (let-values (((x+1 something)
                      (if (> (real 2) 1)
                          (values (+ 1 x) (+ 4 x))
                          (values (+ 1 x) (+ 3 x)))))
          (let ((y (+ 1 x)))
            (* something (+ y x+1)))))))

   ;; Do not collapse procedures that have side effects
   (equal?
    '(let ((x (read-real))
           (y (read-real)))
       (+ x y))
    (%intraprocedural-cse
     '(let ((x (read-real))
            (y (read-real)))
        (+ x y))))

   ;; But do collapse their outputs, once bound
   (equal?
    '(let ((x (read-real)))
       (+ x x))
    (intraprocedural-cse
     '(let ((x (read-real)))
        (let ((y x))
          (+ x y)))))

   ;; Do not collapse user procedures that have side effects
   (equal?
    '(begin
       (define (my-read)
         (read-real))
       (let ((x (my-read))
             (y (my-read)))
         (+ x y)))
    (%intraprocedural-cse
     '(begin
        (define (my-read)
          (read-real))
        (let ((x (my-read))
              (y (my-read)))
          (+ x y)))))

   ;; Do not collapse user procedures that have side effects even if
   ;; they return multiple values
   (equal?
    '(begin
       (define (foo)
         (values (read-real) (read-real)))
       (let-values (((x y) (foo)))
         (let-values (((z w) (foo)))
           (+ x (+ y (+ z w))))))
    (%intraprocedural-cse
     '(begin
        (define (foo)
          (values (read-real) (read-real)))
        (let-values (((x y) (foo)))
          (let-values (((z w) (foo)))
            (+ x (+ y (+ z w))))))))

   ;; Do not collapse expressions with side-effects even if the side-effects
   ;; are hidden by layers of other stuff
   (equal?
    '(let ((x (+ (read-real) (read-real)))
           (y (+ (read-real) (read-real))))
       (+ x y))
    (%intraprocedural-cse
     '(let ((x (+ (read-real) (read-real)))
            (y (+ (read-real) (read-real))))
        (+ x y))))

   ;; Do not collapse IFs whose branches have side-effects.
   (equal?
    '(let ((x (if (< (real 1) 3)
                  (read-real)
                  2))
           (y (if (< (real 1) 3)
                  (read-real)
                  2)))
       (+ x y))
    (%intraprocedural-cse
     '(let ((x (if (< (real 1) 3)
                   (read-real)
                   2))
            (y (if (< (real 1) 3)
                   (read-real)
                   2)))
        (+ x y))))

   ;; Do not collapse structures whose slots are filled directly with
   ;; side-effects.
   (equal?
    '(let ((x (cons 1 (read-real)))
           (y (cons 1 (read-real))))
       (cons x y))
    (%intraprocedural-cse
     '(let ((x (cons 1 (read-real)))
            (y (cons 1 (read-real))))
        (cons x y))))

   ;; In this example, CSE alone will not be able to simplify an
   ;; occurrence of (car x), but if you do ANF first, it will.
   (equal?
    1
    (%eliminate-intraprocedural-dead-code
     (%intraprocedural-cse
      (approximate-anf
       '(let ((x (cons 1 (read-real))))
          (car x))))))

   ;; Do not collapse calls to REAL -- we're not supposed to know what
   ;; the constant inside is.
   (equal?
    '(let ((x (real 1)))
       (let ((y (real 1)))
         (+ x y)))
    (intraprocedural-cse
     '(let ((x (real 1)))
        (let ((y (real 1)))
          (+ x y)))))

   ;; Algebraic simplification can expose CSE opportunities,
   ;; even from nice simplifications like (* foo 1) -> foo
   (equal?
    '(let ((foo (real 2)))
       (let ((bar (+ 1 foo)))
         (+ bar bar)))
    (intraprocedural-cse
     '(let ((foo (real 2)))
        (let ((bar (+ 1 foo)))
          (let ((x (* 1 foo)))
            (+ bar (+ 1 x)))))))

   ;; CSE should see though (and canonicalize) order of attaching
   ;; constants
   (equal?
    '(let ((foo (real 2)))
       (let ((bar (+ 1 foo)))
         (+ bar bar)))
    (intraprocedural-cse
     '(let ((foo (real 2)))
        (let ((bar (+ foo 1)))
          (let ((x (* foo 1)))
            (+ bar (+ 1 x)))))))

   ;; Lambda expressions are not the same as their bodies
   (equal?
    '(let ((y (real 3)))
       (let ((z (lambda (formal)
                  (type (escaper real real))
                  y)))
         z))
    (intraprocedural-cse
     '(let ((y (real 3)))
        (let ((z (lambda (formal)
                  (type (escaper real real))
                   y)))
          z))))

   ;; Treating CONS as a pure procedure collapses EQUAL? pairs into
   ;; structure sharing.
   (equal?
    '(let ((x (cons 1 2)))
       (cons x x))
    (intraprocedural-cse
     '(let ((x (cons 1 2)))
        (let ((y (cons 1 2)))
          (cons x y)))))

   ;; With appropriate simplifications, CSE works through locally
   ;; constructed structures.
   (equal?
    '(let ((x (cons 1 2)))
       1)
    (intraprocedural-cse
     '(let ((x (cons 1 2)))
        (let ((y (car x)))
          y))))

   (equal?
    '(let ((z (real 1)))
       (let ((x (cons z 2)))
         z))
    (intraprocedural-cse
     '(let ((z (real 1)))
        (let ((x (cons z 2)))
          (let ((y (car x)))
            y)))))

   ;; TODO An example for interprocedural CSE
   #;
   (begin
     (define (double-fact n1 n2)
       (argument-types real real (values real real))
       (if (<= n1 1)
           (values n1 n2)
           (let-values (((n1-1! n2-1!) (double-fact (- n1 1) (- n2 1))))
             (values (* n1 n1-1!) (* n2 n2-1!)))))
     (let ((x (real 5)))
       (let-values (((a1 a2) (double-fact x x)))
         (cons a1 a2))))

   ))
