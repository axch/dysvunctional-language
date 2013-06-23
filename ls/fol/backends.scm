(declare (usual-integrations))

;;;; Abstraction of the concept of a "backend"

;;; To facilitate selecting them dynamically, and checking whether a
;;; given operation is possible.

(define-structure backend
  name
  synopsis
  compile
  execute)

(define the-backends '())

(define-syntax define-backend
  (syntax-rules ()
    ((_ name arg ...)
     (begin
       (define name (make-backend 'name arg ...))
       (set! the-backends
             (append the-backends (list (cons 'name name))))))))

(define-backend mit-scheme
  "Compile to native code via MIT Scheme"
  fol->mit-scheme run-mit-scheme)

(define-backend floating-mit-scheme
  "Like mit-scheme, but force floating-point arithmetic"
  fol->floating-mit-scheme run-mit-scheme)

(define-backend standalone-mit-scheme
  "Like mit-scheme, but include FOL runtime"
  fol->standalone-mit-scheme #f)

(define-backend stalin
  "Compile to native code via the Stalin Scheme compiler"
  fol->stalin #f)

(define-backend common-lisp
  "Generate Common Lisp and compile to native code via SBCL"
  fol->common-lisp run-common-lisp)
