;;;; Stalin runtime system

;;; Here is the complement of definitions that needs to be loaded into
;;; Stalin in order to compile FOL code.

(define (real x)
  x)

(define read-real read)

(define (write-real x)
  (write x)
  (newline)
  x)

(define *the-gensym* 0)

(define (gensym)
  (set! *the-gensym* (+ *the-gensym* 1))
  (- *the-gensym* 1))

(define (gensym= gensym1 gensym2)
  (= gensym1 gensym2))

(define values list)

(define (call-with-values values receiver)
  (apply receiver values))
