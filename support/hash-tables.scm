;;; ----------------------------------------------------------------------
;;; Copyright 2010-2011 National University of Ireland
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

(define (hash-table/fold-with-key procedure initial hash-table)
  (let ((result initial))
    (hash-table/for-each
     hash-table
     (lambda (key value)
       (set! result (procedure key value result))))
    result))

(define (hash-table/fold procedure initial hash-table)
  (hash-table/fold-with-key
   (lambda (_ value result)
     (procedure value result))
   initial
   hash-table))

(define (hash-table/insert-with! procedure key new-value hash-table)
  (hash-table/lookup
   hash-table
   key
   (lambda (old-value)
     (hash-table/put! hash-table key (procedure new-value old-value)))
   (lambda ()
     (hash-table/put! hash-table key new-value))))

(define (hash-table/adjust-with-key! procedure key hash-table)
  (hash-table/lookup
   hash-table
   key
   (lambda (value)
     (hash-table/put! hash-table key (procedure key value)))
   (lambda () #f)))

(define (hash-table/adjust! procedure key hash-table)
  (hash-table/adjust-with-key!
   (lambda (_ value) (procedure value))
   key
   hash-table))

(define (hash-table/put-alist! table alist)
  (for-each (lambda (k.v)
              (hash-table/put! table (car k.v) (cdr k.v)))
            alist))

(define (alist->eq-hash-table alist)
  (abegin1
   (make-eq-hash-table)
   (hash-table/put-alist! it alist)))
