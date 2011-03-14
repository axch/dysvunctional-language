(declare (usual-integrations))
;;;; Scheme names for generated code pieces

;;; Nothing to see here.

(define *symbol-count* 0)

(define (make-name prefix)
  (set! *symbol-count* (+ *symbol-count* 1))
  (symbol (strip-trailing-counts prefix) '- *symbol-count*))

(define (strip-trailing-counts symbol)
  (let* ((the-string (symbol->string symbol))
         (prefix-end (re-match-start-index 0 (re-string-search-forward "[-0-9]*$" the-string)))
         (prefix (substring the-string 0 prefix-end)))
    prefix))

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
     (let ((answer (make-name 'closure-)))
       (hash-table/put! *closure-names* closure answer)
       answer))))

(define (abstract-closure->scheme-constructor-name closure)
  (symbol 'make- (abstract-closure->scheme-structure-name closure)))

(define *call-site-names* (make-abstract-hash-table))

(define (call-site->scheme-function-name closure abstract-arg)
  (hash-table/lookup *call-site-names* (cons closure abstract-arg)
   (lambda (value) value)
   (lambda ()
     (let ((answer (make-name 'operation-)))
       (hash-table/put! *call-site-names*
        (cons closure abstract-arg) answer)
       answer))))

(define (initialize-name-caches!)
  (set! *symbol-count* 0)
  (set! *closure-names* (make-abstract-hash-table))
  (set! *call-site-names* (make-abstract-hash-table)))

(define (symbol-with-prefix? thing prefix)
  (and (symbol? thing)
       (let ((name (symbol->string thing)))
         (and (> (string-length name) (string-length prefix))
              (equal? (string-head name (string-length prefix))
                      prefix)))))

(define (generated-temporary? thing)
  (symbol-with-prefix? thing "temp-"))
