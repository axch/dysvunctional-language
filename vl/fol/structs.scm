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
      (receive
       (structure-name? constructor? access-index accessor?)
       (structures-map program)
       (define fix-types
         (on-subexpressions
          (rule `(? type ,structure-name?) 'vector)))
       (define (fix-body expr)
         ((on-subexpressions
           (rule `((? operator ,accessor?) (? operand))
                 `(vector-ref ,operand ,(access-index operator))))
          ((on-subexpressions
            (rule `(? name ,constructor?) 'vector))
           expr)))
       (define fix-definition
         (rule `(define (? formals)
                  (argument-types (?? arg-types))
                  (?? body))
               `(define ,formals
                  (argument-types ,@(fix-types arg-types))
                  ,@(fix-body body))))
       (append
        (map fix-definition
             (filter (lambda (x) (not (structure-definition? x)))
                     (except-last-pair program)))
        (list (fix-body (car (last-pair program))))))
      program))

(define (structures-map program)
  (let* ((structure-definitions (filter structure-definition? program))
         (structure-names (map cadr structure-definitions))
         (structure-name-map
          (alist->eq-hash-table
           (map (lambda (name) (cons name #t)) structure-names)))
         (constructor-map
          (alist->eq-hash-table
           (map (lambda (name) (cons (symbol 'make- name) #t)) structure-names)))
         (accessor-map
          (alist->eq-hash-table
           (append-map
            (lambda (defn)
              (map (lambda (field index)
                     (cons (symbol (cadr defn) '- field) index))
                   (cddr defn)
                   (iota (length (cddr defn)))))
            structure-definitions))))
    (define (structure-name? name)
      (hash-table/get structure-name-map name #f))
    (define (constructor? name)
      (hash-table/get constructor-map name #f))
    (define (access-index name)
      (hash-table/get accessor-map name #f))
    (define (accessor? name)
      (not (not (access-index name))))
    (values structure-name? constructor? access-index accessor?)))
