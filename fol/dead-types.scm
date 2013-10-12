;;; ----------------------------------------------------------------------
;;; Copyright 2013 Alexey Radul.
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
(declare (integrate-external "syntax"))
(declare (integrate-external "../support/pattern-case/pattern-case"))
;;;; Dead type elimination

;;; Eliminating unused type declarations

(define (%dead-type-elimination program)
  (if (begin-form? program)
      (let ((live-types (make-eq-hash-table))
            (queue '())
            (type-map (type-map program)))
        (define (live-type! type)
          (cond ((null? type) 'ok)
                ((symbol? type)
                 (hash-table/lookup live-types type
                  (lambda (datum)
                    'ok)
                  (lambda ()
                    (hash-table/put! live-types type #t)
                    (set! queue (cons type queue)))))
                (else
                 (for-each live-type! (type-references type)))))
        (define (a-live-type!)
          (if (null? queue)
              #f
              (begin1 (car queue)
                (set! queue (cdr queue)))))
        (for-each-fol-expression program
         (lambda (expression type)
           (live-type! type)))
        (for-each
         (rule `(define (? stuff)
                  (argument-types (?? types))
                  (? body))
               (for-each live-type! types))
         program)
        (let loop ((next (a-live-type!)))
          (if next
              (begin
                (live-type! (hash-table/get type-map next #f))
                (loop (a-live-type!)))))
        (tidy-begin
         (filter (lambda (item)
                   (or (not (type-definition? item))
                       (hash-table/get live-types (cadr item) #f)))
                 program)))
      program))
