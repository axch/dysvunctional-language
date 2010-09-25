(define (variable? thing)
  (or (symbol? thing)
      (boolean? thing)
      (number? thing)))

(define (constant? thing)
  (and (variable? thing)
       (not (symbol? thing))))

(define-structure (closure (safe-accessors #t))
  formal
  body
  env)

(define-structure (primitive (safe-accessors #t))
  name
  implementation
  abstract-implementation)

(define (flow-value->scheme-value thing)
  thing)
