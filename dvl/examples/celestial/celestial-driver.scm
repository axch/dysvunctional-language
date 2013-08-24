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

;;; To play with this, you need the ScmUtils library (for the
;;; graphics).  Be sure to start it with enough heap and stack space
;;; (--heap 160000 --stack 40000 works on 64-bit Linux).  Then
;;; - load dvl with (load "load")
;;; - load this file
;;; - compile the dvl program
;;; - execute an integration

;;; To compile from the REPL (be sure to start Scheme with enough heap
;;; and stack space; see the dvl script)
#;
 (fol->mit-scheme
  (fol-optimize ; Optimize a second time
   (compile-to-fol (dvl-source "examples/celestial/celestial.dvl") visibly))
  "examples/celestial/celestial")

;;; To compile from the command line
;$ dvl compile example/celestial/celestial.dvl optimizing twice

;;; To run, do something like this:
; (go 1000 10. 100.)

(define (go num-samples time-step num-steps-per-sample)
  (stream-take
   num-samples
   (stream-for-each
    (plot-objects window)
    ((constant-arg-for-dvl-stream (exact->inexact time-step))
     ((run-mit-scheme compiled-path)
      (exact->inexact num-steps-per-sample))))))

(define compiled-path
  (string-append
   (->namestring (self-relatively working-directory-pathname))
   "celestial"))

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
