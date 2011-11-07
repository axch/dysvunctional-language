(declare (usual-integrations))

;;;; Runtime system

;;; Here is the complement of definitions that needs to be loaded into
;;; MIT Scheme in order to execute FOL code (in addition to SRFI 11).
;;; See also mit-scheme.scm and FOL-EVAL in syntax.scm.

(define-syntax argument-types
  (syntax-rules ()
    ((_ arg ...)
     (begin))))

(define-syntax define-typed-structure
  (syntax-rules ()
    ((_ name (field type) ...)
     (define-structure name field ...))))

(define (real x)
  (if (real? x)
      x
      (error "A non-real object is asserted to be real" x)))

(define read-real read)

(define (write-real x)
  (write x)
  (newline)
  x)

(define-structure
  (gensym
   safe-accessors
   (print-procedure
    (simple-unparser-method 'gensym
     (lambda (gensym)
       (list (gensym-number gensym))))))
  number)

(define *the-gensym* 0)

(define (gensym)
  (set! *the-gensym* (+ *the-gensym* 1))
  (make-gensym (- *the-gensym* 1)))

(define (gensym= gensym1 gensym2)
  (= (gensym-number gensym1) (gensym-number gensym2)))

(define (gensym< gensym1 gensym2)
  (< (gensym-number gensym1) (gensym-number gensym2)))
