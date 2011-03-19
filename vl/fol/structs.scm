(declare (usual-integrations))
;;;; Turning record structures into vectors

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
;;; accessor, needs to give the index of the field accessed.

(define (structures-map program)
  (let* ((structure-definitions (filter structure-definition? program))
         (structure-names (map cadr structure-definitions))
         (structure-name-map
          (alist->eq-hash-table
           (map (lambda (name) (cons name #t)) structure-names))))
    (hash-table/put-alist!
     structure-name-map
     (map (lambda (name) (cons (symbol 'make- name) #t)) structure-names))
    (hash-table/put-alist!
     structure-name-map
     (append-map
      (lambda (defn)
        (map (lambda (field index)
               (cons (symbol (cadr defn) '- field) index))
             (cddr defn)
             (iota (length (cddr defn)))))
      structure-definitions))
    (define (classify name)
      (hash-table/get structure-name-map name #f))
    classify))
