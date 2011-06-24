(declare (usual-integrations))
;;;; Scheme names for generated code pieces

;;; Nothing to see here.

(define (vl-variable->scheme-variable var) var)

(define (vl-variable->scheme-field-name var) var)

(define (vl-variable->scheme-record-access var closure)
  `(,(symbol (abstract-closure->scheme-structure-name closure)
             '- (vl-variable->scheme-field-name var))
    the-closure))

(define (fresh-temporary)
  (make-name 'temp))

(define *closure-names* (make-abstract-hash-table))

(define (abstract-closure->scheme-structure-name closure)
  (hash-table/intern! *closure-names* closure
   (lambda () (name->symbol (make-name 'closure)))))

(define (abstract-closure->scheme-constructor-name closure)
  (symbol 'make- (abstract-closure->scheme-structure-name closure)))

(define *call-site-names* (make-abstract-hash-table))

(define (call-site->scheme-function-name closure abstract-arg)
  (hash-table/intern! *call-site-names* (cons closure abstract-arg)
   (lambda () (name->symbol (make-name 'operation)))))

(define (clear-name-caches!)
  (set! *closure-names* (make-abstract-hash-table))
  (set! *call-site-names* (make-abstract-hash-table))
  ;; Also poke memoized functions to clear out table structure I may
  ;; have just freed
  (gc-flip)
  (if (lexical-unbound? (->environment clear-name-caches!) 'abstract-hash)
      'ok
      (abstract-hash 0)))

(define (initialize-name-caches!)
  (set! *closure-names* (make-abstract-hash-table))
  (set! *call-site-names* (make-abstract-hash-table))
  (reset-fol-names!))
