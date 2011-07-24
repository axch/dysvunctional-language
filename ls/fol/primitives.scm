(declare (usual-integrations))
;;;; FOL Primitivies

(define-structure (primitive safe-accessors)
  name
  type
  pure?)

(define (primitive-impure? primitive)
  (not (primitive-pure? primitive)))

(define-structure
  (function-type
   (constructor function-type)
   safe-accessors
   (print-procedure
    (lambda (state object)
      (with-current-unparser-state state
        (lambda (port)
          (display "(-> " port)
          (write (arg-types object) port)
          (display " " port)
          (write (return-type object) port)
          (display ")" port))))))
  args
  return)

(define return-type function-type-return)
(define arg-types function-type-args)

(define (real->real thing)
  (make-primitive thing (function-type '(real) 'real) #t))
(define (real*real->real thing)
  (make-primitive thing (function-type '(real real) 'real) #t))
(define (real->bool thing)
  (make-primitive thing (function-type '(real) 'bool) #t))
(define (real*real->bool thing)
  (make-primitive thing (function-type '(real real) 'bool) #t))

;; Type testers real? gensym? null? pair? procedure? have other types, but
;; should never be emitted by VL or DVL on union-free inputs.

(define *primitives*
  `(,(make-primitive 'read-real (function-type '() 'real) #f)
    ,(make-primitive 'write-real (function-type '(real) 'real) #f)
    ,@(map real->real
           '(abs exp log sin cos tan asin acos sqrt real))
    ,@(map real*real->real '(+ - * / atan expt))
    ,@(map real->bool '(zero? positive? negative?))
    ,@(map real*real->bool '(< <= > >= =))
    ,(make-primitive 'gensym (function-type '() 'gensym) #f)
    ,(make-primitive 'gensym= (function-type '(gensym gensym) 'bool) #t)))
