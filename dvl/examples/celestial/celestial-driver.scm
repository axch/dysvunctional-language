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

(define (constant-arg-for-dvl-stream argument)
  (lambda (dvl-stream)
    (let loop ((dvl-stream dvl-stream))
      (cons (car dvl-stream)
            (delay (loop ((cdr dvl-stream) argument)))))))

(define (stream-for-each f stream)
  (cons (f (car stream))
        (delay (stream-for-each f (force (cdr stream))))))

(define ((plot-object window) object)
  (let ((position (cadr object)))
    (let ((x (car position))
          (y (cadr position)))
      ;(pp `(object-at ,x ,y ,z))
      ;(pp `(speed ,@(caddr object)))
      ; Drop z coord in plot
      (plot-point window x y))))

(define ((plot-objects window) state)
  (for-each (plot-object window) (cdr state)))

(define window (frame -60 60 -60 60))

(define (stream-take count stream)
  (if (= count 0)
      stream
      (stream-take (- count 1) (force (cdr stream)))))

(define (compare state1 state2)
  (cond ((pair? state1)
         (cons (compare (car state1) (car state2))
               (compare (cdr state1) (cdr state2))))
        ((null? state1)
         '())
        (else (/ (- state1 state2) state1))))

;; Compile the DVL program with
;(fol->mit-scheme (compile-to-fol (dvl-source "examples/celestial/celestial.dvl") visibly))
;; or compile the DVL program with
;$ dvl compile example/celestial/celestial.dvl
;; at the command line.

;; If the MIT Scheme compiler complains with an interminable pile of
;Warning: Wrong number of arguments #[liar:procedure 83 lambda-2010] (#[liar:reference 71 |#[continuation]|] #[liar:reference 72 #[uninterned-symbol 73 value-0]] #[liar:reference 74 #[uninterned-symbol 75 value-1]] #[liar:reference 76 #[uninterned-symbol 77 value-2]] #[liar:reference 78 #[uninterned-symbol 79 value-3]] ...)
;; then add an extra round of fol optimization, thus:
;(fol->mit-scheme (fol-optimize (compile-to-fol (dvl-source "examples/celestial/celestial.dvl") visibly)))
;; or thus:
;$ dvl compile example/celestial/celestial.dvl optimizing twice

;; Then run with
#;
(stream-take
 1000
 (stream-for-each
  (plot-objects window)
  ((constant-arg-for-dvl-stream 10.)
   ((run-mit-scheme) 100.))))
