(declare (usual-integrations))
;;;; Scheme names for generated code pieces

;;; Nothing to see here.

(define *symbol-count* 0)

(define-structure
  (fol-name
   safe-accessors
   (print-procedure
    (simple-unparser-method 'fol-name
     (lambda (object)
       (list (fol-name-base object) (fol-name-count object))))))
  base
  count)

(define (name-base thing)
  (cond ((fol-name? thing)
         (fol-name-base thing))
        ((symbol? thing) thing)
        (else (error "Invalid name" thing))))

(define (make-name template)
  (set! *symbol-count* (+ *symbol-count* 1))
  (make-fol-name (name-base template) *symbol-count*))

(define (fol-var? thing)
  (or (symbol? thing)
      (fol-name? thing)))

(define (name->symbol thing)
  (cond ((fol-name? thing)
         (symbol (fol-name-base thing) '- (fol-name-count thing)))
        ((symbol? thing) thing)
        (else "Invalid var" thing)))

(define prepare-for-scheme
  (rule-simplifier
   (list
    (rule `(? name ,fol-name?) (name->symbol name)))))

(define (vl-variable->scheme-variable var) var)

(define (vl-variable->scheme-field-name var) var)

(define (vl-variable->scheme-record-access var closure)
  `(,(symbol (abstract-closure->scheme-structure-name closure)
             '- (vl-variable->scheme-field-name var))
    the-closure))

(define (fresh-temporary)
  (make-name 'temp-))

(define *closure-names* (make-abstract-hash-table))

(define (abstract-closure->scheme-structure-name closure)
  (hash-table/lookup *closure-names* closure
   (lambda (value) value)
   (lambda ()
     (let ((answer (name->symbol (make-name 'closure))))
       (hash-table/put! *closure-names* closure answer)
       answer))))

(define (abstract-closure->scheme-constructor-name closure)
  (symbol 'make- (abstract-closure->scheme-structure-name closure)))

(define *call-site-names* (make-abstract-hash-table))

(define (call-site->scheme-function-name closure abstract-arg)
  (hash-table/lookup *call-site-names* (cons closure abstract-arg)
   (lambda (value) value)
   (lambda ()
     (let ((answer (name->symbol (make-name 'operation))))
       (hash-table/put! *call-site-names*
        (cons closure abstract-arg) answer)
       answer))))

(define (initialize-name-caches!)
  (set! *symbol-count* 0)
  (set! *closure-names* (make-abstract-hash-table))
  (set! *call-site-names* (make-abstract-hash-table)))
