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
;;;; FOL Syntax

(define ((tagged-list? tag) thing)
  (and (pair? thing)
       (eq? (car thing) tag)))

;;; Begin

(define begin-form? (tagged-list? 'begin))

(define (tidy-begin form)
  (if (and (begin-form? form) (null? (cddr form)))
      (cadr form)
      form))

(define (definitions program)
  (if (begin-form? program)
      (except-last-pair (cdr program))
      '()))

(define (entry-point program)
  (if (begin-form? program)
      (last program)
      program))

;;; Define

(define procedure-definition? (tagged-list? 'define))

(define (normalize-definition definition)
  (cond ((not (procedure-definition? definition))
         (error "Trying to normalize a non-definition" definition))
        ((pair? (cadr definition))
         (normalize-definition
          `(define ,(caadr definition)
             (lambda ,(cdadr definition)
               ,@(cddr definition)))))
        (else definition)))

(define (definiendum definition)
  (cadr (normalize-definition definition)))

(define (definiens definition)
  (caddr (normalize-definition definition)))

(define reconstitute-definition
  (iterated
   (rule `(define (? name)
            (lambda (? names)
              (?? body)))
         `(define (,name ,@names)
            ,@body))))

;; Wins with formals, type declaration, and body
(define-algebraic-matcher definition procedure-definition? cadr caddr cadddr)

;;; Simple forms

(define (fol-var? thing)
  (or (symbol? thing)
      (fol-name? thing)))
(define-algebraic-matcher fol-var fol-var? id-project)

(define (fol-const? thing)
  (or (number? thing) (boolean? thing) (null? thing)))
(define-algebraic-matcher fol-const fol-const? id-project)

(define (simple-form? thing)
  (or (fol-var? thing) (number? thing) (boolean? thing) (null? thing)))
(define-algebraic-matcher simple-form simple-form? id-project)

;;; If

(define if-form? (tagged-list? 'if))
(define-algebraic-matcher if-form if-form? cadr caddr cadddr)

;;; Binders

(define-integrable (cadaadr thing)
  (cadr (caadr thing)))
(define let-form? (tagged-list? 'let))
(define-algebraic-matcher let-form let-form? cadr caddr)
(define let-values-form? (tagged-list? 'let-values))
;; Wins with names, subexpression, and body.  Assumes one binding form
(define-algebraic-matcher let-values-form let-values-form? caaadr cadaadr caddr)
(define lambda-form? (tagged-list? 'lambda))
(define-algebraic-matcher lambda-form lambda-form? cadr caddr)

(define (binder-tag? thing)
  (or (eq? thing 'let)
      (eq? thing 'let-values)))

;; N.B. These interconverters of binding forms are no longer quite
;; right, in light of lambda meaning "escaping function" in FOL.  To
;; wit, the ->lambda rule does not insert a type declaration into the
;; new lambda (how could it?) and ->let{-values} does not remove one.
;; Currently, this is correct (if confusing) because these converters
;; are only used to increase the uniformity of treatment of binders.
;; Perhaps there is a nicer way to achieve the same effect.
(define ->lambda
  (rule-list
   (list
    (rule `(let (? bindings) (?? body))
          `((lambda ,(map car bindings)
              ,@body)
            ,@(map cadr bindings)))
    ;; This does not produce a syntactically valid object, but it does
    ;; accurately capture what the names are doing.
    (rule `(let-values (((? names) (? exp))) (?? body))
          `((lambda ,names ,@body)
            ,exp)))))

(define ->let
  (rule `((lambda (? names) (?? body)) (?? args))
        `(let ,(map list names args) ,@body)))

(define ->let-values
  (rule `((lambda (? names) (?? body)) (? exp))
        `(let-values ((,names ,exp))
           ,@body)))

(define (tidy-empty-let form)
  (if (and (let-form? form)
           (null? (cadr form)))
      (caddr form)
      form))

;; Converting LET to LET* is only useful for viewing a program, as the
;; result is not valid FOL any more.  It can, however, be converted
;; back by let*->let, below.
(define let->let*
  (rule-simplifier
   (list
    (rule `(let ((? binding))
             (let ((? binding2))
               (?? body)))
          `(let* (,binding
                  ,binding2)
             ,@body))
    (rule `(let ((? binding))
             (let* ((?? bindings))
               (?? body)))
          `(let* (,binding
                  ,@bindings)
             ,@body)))))

(define let*-form? (tagged-list? 'let*))
(define-algebraic-matcher let*-form let*-form? cadr caddr)

(define let*->let
  (rule-simplifier
   (list
    (rule `(let* ((? binding))
             (?? body))
          `(let (,binding)
             ,@body))
    (rule `(let* ((? binding)
                  (?? bindings))
             (?? body))
          `(let (,binding)
             (let* (,@bindings)
               ,@body))))))

;;; Structures

(define (accessor? expr)
  (or (cons-ref? expr)
      (vector-ref? expr)
      (implicit-acessor? expr)))
; Produces accessor, object, and extra (that being either a null or a
; list containing the index for vector-ref).
(define-algebraic-matcher accessor accessor? car cadr cddr)

(define (cons-ref? expr)
  (and (pair? expr)
       (memq (car expr) '(car cdr))))

(define vector-ref? (tagged-list? 'vector-ref))

(define (implicit-acessor? expr)
  (and (pair? expr)
       *accessor-constructor-map*
       (integer? (*accessor-constructor-map* (car expr)))))

(define (access-index access-form)
  (cond ((eq? (car access-form) 'car) 0)
        ((eq? (car access-form) 'cdr) 1)
        ((eq? (car access-form) 'vector-ref)
         (caddr access-form))
        ((and *accessor-constructor-map*
              (integer? (*accessor-constructor-map* (car access-form))))
         (*accessor-constructor-map* (car access-form)))
        (else (error "Not a valid accessor" (car access-form)))))

(define (select-by-access thing access-form)
  (list-ref (cdr thing) (access-index access-form)))

(define (construction? expr)
  (or (and (pair? expr)
           (memq (car expr) '(cons vector)))
      (implicit-construction? expr)))
(define-algebraic-matcher construction construction? car cdr)

(define (implicit-construction? expr)
  (and (pair? expr)
       *accessor-constructor-map*
       (eq? 'constructor (*accessor-constructor-map* (car expr)))))

(define values-form? (tagged-list? 'values))
(define-algebraic-matcher values-form values-form? cdr)

(define (tidy-values exp)
  (if (and (values-form? exp)
           (= 2 (length exp)))
      (cadr exp)
      exp))

(define (smart-values-subforms form)
  (if (values-form? form)
      (cdr form)
      (list form)))

(define (append-values values-forms)
  (tidy-values
   `(values ,@(append-map smart-values-subforms values-forms))))

;;; Measurements

(define (print-fol-size program)
  (let ((size (count-pairs program))
        (stripped-size (count-pairs (strip-argument-types program)))
        (struct-size (count-pairs (filter structure-definition? program))))
    (format #t "~A pairs + ~A pairs of structure definitions + ~A pairs of type annotations"
            (- stripped-size struct-size) struct-size (- size stripped-size))
    (newline))
  program)

(define (print-short-fol-stats program)
  (let* ((program (if (begin-form? program)
                      program
                      `(begin ,program)))
         (defns (filter procedure-definition? program))
         (defn-count (length defns))
         (structure-defns (filter structure-definition? program))
         (struct-count (length structure-defns)))
    (print-fol-size program)
    (format #t "~A procedures" defn-count)
    (when (> struct-count 0)
      (format #t " + ~A structures" struct-count))
    (newline)))

(define (print-fol-statistics program)
  (define defn-statistics
    (rule `(define ((? name) (?? formals))
             (? arg-types)
             (? body))
          (list
           (length formals) (count-pairs body) (count-pairs arg-types))))
  (define (display-list-statistics items description)
    (if (not (null? items))
        (let ((sorted (sort items >)))
          (let ((mean (list-ref sorted (quotient (length items) 2)))
                (top (take sorted (min 10 (length sorted)))))
            (format #t "mode ~A: ~A\ntop ~As: ~A\n"
                    description mean description top)))))
  (let* ((program (if (begin-form? program)
                      program
                      `(begin ,program)))
         (defns (filter procedure-definition? program))
         (defn-stats (map defn-statistics defns))
         (body-sizes (cons (count-pairs (last program))
                           (map cadr defn-stats)))
         (formal-lengths (map car defn-stats))
         (type-decl-sizes (map caddr defn-stats)))
    (print-short-fol-stats program)
    (display-list-statistics body-sizes "expression size")
    (display-list-statistics formal-lengths "formal length")
    (display-list-statistics type-decl-sizes "type annotation size")
    (flush-output)))

(define (count-primitives program)
  (let ((names (map primitive-name *primitives*)))
    (let loop ((program program))
      (cond ((pair? program)
             (+ (loop (car program))
                (loop (cdr program))))
            ((null? program) 0)
            ((memq program names) 1)
            (else 0)))))

;;; Flushing type signatures

;; Flushing type signatures is only useful for viewing a program, as
;; the result is not valid FOL any more.

(define remove-defn-argument-types
  (rule `(define (? formals)
           (argument-types (?? etc))
           (?? body))
        `(define ,formals
           ,@body)))

(define %remove-lambda-type-declarations
  (on-subexpressions
   (rule `(lambda (? formals)
            (type (? something))
            (?? body))
         `(lambda ,formals
            ,@body))))

(define (strip-argument-types program)
  (%remove-lambda-type-declarations
   (if (begin-form? program)
       (map remove-defn-argument-types program)
       program)))

;;; Reserved words

(define fol-reserved '(cons car cdr vector vector-ref begin define-type define if let let-values values))

;;;; "Runtime system"

(define (fol-eval code)
  (eval (prepare-for-scheme code) fol-environment))
