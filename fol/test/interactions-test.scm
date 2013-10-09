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
 interactions

 (define-test (sra-then-inline)
   ;; SRA does ANF conversion, which can create aliases that inlining
   ;; first would have avoided.
   (define program
     '(begin
        (define (inlinee x)
          (argument-types real real)
          (* x x))
        (inlinee (real 1))))
   (check
    (equal?
     '(let ((x (real 1)))
        (* x x))
     (scalar-replace-aggregates (inline program))))
   (check
    (alpha-rename?
     '(let ((anf-42 (real 1)))
        (let ((x anf-42))
          (* x x)))
     (inline (scalar-replace-aggregates program))))
   (check
    (alpha-rename?
     '(let ((x (real 1)))
        (* x x))
     (intraprocedural-cse
      (inline
       (scalar-replace-aggregates program))))))

 (define-test (sra-then-dead-code)
   (define program
     '(let ((x (* (real 2) 2)))
        (car (cons (real 1) x))))
   ;; Dead code cannot kill structure slots
   (check (equal? program
                  (interprocedural-dead-code-elimination program)))
   ;; unless they are exposed to it by SRA
   (check (equal? '(real 1)
                  (reverse-anf
                   (interprocedural-dead-code-elimination
                    (scalar-replace-aggregates program))))))

 (define-test (dead-code-then-inline)
   ;; Eliminating dead code may open inlining opportunities by
   ;; deleting edges in the call graph.
   (define program
     '(begin
        (define (nominally-recursive x)
          (argument-types real real)
          (let ((foo (if (< x 0)
                         x
                         (nominally-recursive (- x 1)))))
            x))
        (nominally-recursive (real 5))))
   (check (equal? program (inline program)))
   (check (equal? '(let ((x (real 5))) x)
                  (inline (interprocedural-dead-code-elimination program)))))

 (define-test (lift-lets-then-cse)
   ;; Lifting lets helps CSE because variables spend more time in
   ;; scope.
   (define program
     '(let ((x (real 5)))
        (let ((w (let ((y (+ 3 x))) y)))
          ;; y = (+ 3 x) goes out of scope
          (let ((z (+ 3 x)))
            (+ w z)))))
   (check (equal? program (%intraprocedural-cse program)))
   (check
    (equal?
     '(let ((x (real 5)))
        (let ((y (+ 3 x)))
          (+ y y)))
     (%intraprocedural-cse (lift-lets program)))))

 (define-test (anf-then-cse)
   ;; ANF helps CSE because more subexpressions get names.
   (define program '(let ((x (real 4)))
                      (+ (+ 1 x) (+ 1 x))))
   (check (equal? program (%intraprocedural-cse program)))
   (check
    (alpha-rename?
     '(let ((x (real 4)))
        (let ((y (+ 1 x))
              (z (+ 1 x)))
          (+ y y)))
     (%intraprocedural-cse (approximate-anf program))))
   (check
    (alpha-rename?
     '(let ((x (real 4)))
        (let ((y (+ 1 x)))              ; Gone
          (+ y y)))
     (interprocedural-dead-code-elimination
      (%intraprocedural-cse (approximate-anf program))))))

 )
