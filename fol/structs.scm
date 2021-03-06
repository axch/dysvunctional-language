;;; ----------------------------------------------------------------------
;;; Copyright 2010-2011 National University of Ireland; 2012 Alexey Radul.
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
(declare (integrate-external "../support/pattern-case/load"))
;;;; Turning record structures into vectors

;;; VL and DVL emit code that defines Scheme records to serve as
;;; VL/DVL closure records.  Instead of teaching all the FOL stages how
;;; to deal with each record type, I convert them all to vectors.
;;; This decision will need to be revisited before the introduction of
;;; support for union types.

;;; Every structure definition of the form
;;; (define-type foo (structure (bar bar-type) (baz baz-type)))
;;; entails the constructor MAKE-FOO, accessors FOO-BAR and FOO-BAZ,
;;; and the type FOO.  Fortunately, all are used only in call
;;; position, so they are easy to search for and replace.  The
;;; constructor needs to be replaced with VECTOR, the accessors with
;;; VECTOR-REF and the appropriate index, and the type with the
;;; corresponding fully-expanded tree of VECTOR, CONS, etc.
;;; Thereafter, the structure definitions can be dropped.

(define type-definition? (tagged-list? 'define-type))

(define structure-type? (tagged-list? 'structure))

(define (structure-definition? form)
  (and (type-definition? form)
       (structure-type? (caddr form))))
;; Wins with the name and the slot-type list of the structure
;; definition.
(define-algebraic-matcher
  structure-definition structure-definition? cadr cdaddr)

(define (%structure-definitions->vectors program)
  (if (begin-form? program)
      (tidy-begin
       (let ((classify (structures-map program)))
         (define (accessor? name)
           (integer? (classify name)))
         (define (constructor? name)
           (eq? (classify name) 'constructor))
         (define (type? name)
           (and (classify name)
                (not (accessor? name))
                (not (constructor? name))))
         ((on-subexpressions
           (rule `(? name ,type?)
                 (classify name)))
          ((on-subexpressions
            (rule `((? operator ,accessor?) (? operand))
                  `(vector-ref ,operand ,(classify operator))))
           ((on-subexpressions
             (rule `(? name ,constructor?) 'vector))
            (filter (lambda (x) (not (structure-definition? x)))
                    program))))))
      program))

;;; A structures map needs to say, for every name, whether it is a
;;; constructor, an accessor, a type, or not; if an accessor, needs to
;;; give the index of the field accessed; and if a type, needs to give
;;; the corresponding tree in terms of VECTOR, CONS, and basic types.
;;; I implement this as a procedure that, when given a name, returns
;;; the symbol CONSTRUCTOR if it is a constructor, the tree if it is a
;;; type, the integer field index accessed if it is an accessor, and
;;; #f otherwise.  The procedure in question is backed by a hash
;;; table.

(define (structures-map program)
  (let* ((structure-definitions (filter structure-definition? program))
         (structure-names (map cadr structure-definitions))
         (structure-map (make-eq-hash-table)))
    (hash-table/put-alist!
     structure-map
     (map (lambda-case*
           ((structure-definition name fields)
            (cons name `(vector ,@(map cadr fields)))))
          structure-definitions))
    (define basic-tree
      (lambda (type)
        (cond ((or (null? type) (memq type '(real bool gensym)))
               type)
              ((pair? type)
               (cons (car type) (map basic-tree (cdr type))))
              ((hash-table/get structure-map type #f) => basic-tree))))
    (hash-table/put-alist!
     structure-map
     (map (lambda (type)
            (cons type (basic-tree type)))
          (hash-table/key-list structure-map)))

    (hash-table/put-alist!
     structure-map
     (map (lambda-case*
           ((structure-definition name _)
            (cons (symbol 'make- name) 'constructor)))
          structure-definitions))

    (hash-table/put-alist!
     structure-map
     (append-map
      (lambda-case*
       ((structure-definition name fields)
        (map (lambda (field index)
               (cons (symbol name '- field) index))
             (map car fields)
             (iota (length fields)))))
      structure-definitions))
    (define (classify name)
      (hash-table/get structure-map name #f))
    classify))
