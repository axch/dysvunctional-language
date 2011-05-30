(declare (usual-integrations))
;;;; FOL Primitivies

(define-structure (primitive safe-accessors)
  name
  type)


(define-structure (function-type (constructor function-type) safe-accessors)
  args
  return)

(define return-type function-type-return)
(define arg-types function-type-args)

(define (real->real thing)
  (make-primitive thing (function-type '(real) 'real)))
(define (real*real->real thing)
  (make-primitive thing (function-type '(real real) 'real)))
(define (real->bool thing)
  (make-primitive thing (function-type '(real) 'bool)))
(define (real*real->bool thing)
  (make-primitive thing (function-type '(real real) 'bool)))

;; Type testers real? gensym? null? pair? procedure? have other types, but
;; should never be emitted by VL or DVL on union-free inputs.

(define *primitives*
  `(,(make-primitive 'read-real (function-type '() 'real))
    ,@(map real->real
           '(abs exp log sin cos tan asin acos sqrt write-real real))
    ,@(map real*real->real '(+ - * / atan expt))
    ,@(map real->bool '(zero? positive? negative?))
    ,@(map real*real->bool '(< <= > >= =))
    ,(make-primitive 'gensym (function-type '() 'gensym))
    ,(make-primitive 'gensym= (function-type '(gensym gensym) 'bool))))
