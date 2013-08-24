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

;;; Load this after compiling runge-kutta.dvl with
;;; (fol->floating-mit-scheme (compile-to-fol (dvl-source "examples/runge-kutta/runge-kutta.dvl") visibly))
;;; and feel free to draw some pictures.

(define integrations (run-mit-scheme))

(define euler (car integrations))
(define rk4  (cadr integrations))

(define (integrate integration time-step total-time)
  (let loop ((result '())
             (integration integration))
    (let* ((state (car integration))
           (current-time (car state))
           (func (cdr integration)))
      (if (>= current-time total-time)
          (reverse result)
          (loop (cons state result)
                (func time-step))))))

(define ((relative-error truth) time-series)
  (define (relative-error time estimate)
    (if (>= (abs (truth time)) 1)
        (/ (- (truth time) estimate) (truth time))
        (- (truth time) estimate)))
  (map (lambda (datum)
         (let ((time (car datum))
               (estimate (cadr datum)))
           (cons time (relative-error time estimate))))
       time-series))

#|
 (gnuplot-alist
  ((relative-error sin)
   (integrate euler 0.5 10))
  '(commanding "with points"))
|#
