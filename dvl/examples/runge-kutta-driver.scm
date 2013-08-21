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
;;; (fol->floating-mit-scheme (compile-to-fol (dvl-source "examples/runge-kutta.dvl") visibly))
;;; and feel free to draw some pictures.

(define integrators (run-mit-scheme))

(define euler (car integrators))
(define rk4  (cadr integrators))

(define ((show-1d-ode thing-to-show) integrator time-step total-time)
  (let loop ((result '())
             (integrator integrator))
    (let* ((state (car integrator))
           (current-time (car state))
           (func (cdr integrator)))
      (if (>= current-time total-time)
          (gnuplot-alist (map (lambda (datum)
                                (let ((time (car datum))
                                      (estimate (cadr datum)))
                                  (cons time (thing-to-show time estimate))))
                              (reverse result))
                         '(commanding "with points, sin(x)"))
          (loop (cons state result)
                (func time-step))))))

(define ((relative-error solution) time estimate)
   (if (>= (abs (solution time)) 1)
       (/ (- (solution time) estimate) (solution time))
       (- (solution time) estimate)))

(define (answer time estimate) estimate)

(define show-1d (show-1d-ode answer))
