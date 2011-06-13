(declare (usual-integrations))
;;;; Turning record structures into vectors

;;; VL and DVL emit code that defines Scheme records to serve as
;;; VL/DVL closure records.  Instead of teach all the FOL stages how
;;; to deal with each record type, I convert them all to vectors.
;;; This decision will need to be revisited before the introduction of
;;; support for union types.

;;; Every structure definition of the form
;;; (define-structure foo bar baz)
;;; entails the constructor MAKE-FOO, accessors FOO-BAR and FOO-BAZ,
;;; and the type constructor FOO.  Fortunately, all are used only in
;;; call position, so they are easy to search for and replace.  The
;;; constructor needs to be replaced with VECTOR, the accessors with
;;; VECTOR-REF and the appropriate index, and the type constructor
;;; also with VECTOR.  Thereafter, the structure definitions can be
;;; dropped.

(define (structure-definition? form)
  (and (pair? form)
       (eq? (car form) 'define-structure)))

(define (structure-definitions->vectors program)
  (if (begin-form? program)
      (let ((classify (structures-map program)))
        (define (accessor? name)
          (integer? (classify name)))
        (define (constructor? name)
          (and (classify name)
               (not (integer? (classify name)))))
        ((on-subexpressions
          (rule `((? operator ,accessor?) (? operand))
                `(vector-ref ,operand ,(classify operator))))
         ((on-subexpressions
           (rule `(? name ,constructor?) 'vector))
          (filter (lambda (x) (not (structure-definition? x)))
                  program))))
      program))

;;; A structures map needs to say, for every name, whether it is a
;;; constructor, an accessor, a type constructor, or not; and if an
;;; accessor, needs to give the index of the field accessed.  I
;;; implement this as a procedure that, when given a name, returns the
;;; symbol CONSTRUCTOR if it is a constructor, the symbol
;;; TYPE-CONSTRUCTOR if is a type constructor, the integer field index
;;; accessed if it is an accessor, and #f otherwise.  The procedure
;;; in question is backed by a hash table.

(define (structures-map program)
  (let* ((structure-definitions (filter structure-definition? program))
         (structure-names (map cadr structure-definitions))
         (structure-map (make-eq-hash-table)))
    (hash-table/put-alist!
     structure-map
     (map (lambda (name) (cons (symbol 'make- name) 'constructor))
          structure-names))
    (hash-table/put-alist!
     structure-map
     (map (lambda (name) (cons name 'type-constructor))
          structure-names))
    (hash-table/put-alist!
     structure-map
     (append-map
      (lambda (defn)
        (map (lambda (field index)
               (cons (symbol (cadr defn) '- field) index))
             (cddr defn)
             (iota (length (cddr defn)))))
      structure-definitions))
    (define (classify name)
      (hash-table/get structure-map name #f))
    classify))
