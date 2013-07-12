;;; ----------------------------------------------------------------------
;;; Copyright 2011 National University of Ireland.
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

;;; For CSE, I need an associative data structure that supports
;;; reverse lookups, from value to key.  This is complicated by the
;;; fact that the forward map is many-to-one, but when doing reverse
;;; lookups, I want to find a very specfic one of the many.  This is
;;; implemented by taking a predicate that says which entries to put
;;; into the reverse map.  The predicate accepts two arguments, the
;;; (forward) key and the datum.  For my application, I only need
;;; equal? checking of keys and data, so I will not bother
;;; generalizing on that score.

(define-structure (two-way safe-accessors)
  forward
  reverse
  reverse?)

(define (make-two-way-table reverse?)
  (make-two-way
   (make-equal-hash-table)
   (make-equal-hash-table)
   reverse?))

(define (forward-get table key default)
  (hash-table/get (two-way-forward table) key default))

(define (forward-lookup table key win lose)
  (hash-table/lookup (two-way-forward table) key win lose))

(define (reverse-lookup table datum win lose)
  (hash-table/lookup (two-way-reverse table) datum win lose))

(define (two-way-put! table key datum)
  (hash-table/put! (two-way-forward table) key datum)
  (if ((two-way-reverse? table) key datum)
      (hash-table/put! (two-way-reverse table) datum key)))

(define (two-way-remove! table key)
  (let ((forward (two-way-forward table)))
    (hash-table/lookup forward key
     (lambda (old-datum)
       (if ((two-way-reverse? table) key old-datum)
           (hash-table/remove! (two-way-reverse table) old-datum)))
     (lambda ()
       'ok))
    (hash-table/remove! forward key)))
