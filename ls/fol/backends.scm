(declare (usual-integrations))

;;;; Abstraction of the concept of a "backend"

;;; To facilitate selecting them dynamically, and checking whether a
;;; given operation is possible.

(define-structure backend
  name
  synopsis
  compile
  compiling-problem
  execute
  executing-problem)

(define the-backends '())

(define-syntax define-backend
  (syntax-rules ()
    ((_ name arg ...)
     (begin
       (define name (make-backend 'name arg ...))
       (set! the-backends
             (append the-backends (list (cons 'name name))))))))

(define (executable-present? name)
  (let ((location
         (with-output-to-string
           (lambda ()
             (run-shell-command (string-append "which " name))))))
    (> (string-length location) 0)))

(define always-can (lambda () #f))
(define (never-can msg) (lambda () msg))
(define (needs-executable name)
  (lambda ()
    (if (executable-present? name)
        #f
        (string-append "the " name " program is not on the path"))))

(define-backend mit-scheme
  "Compile to native code via MIT Scheme"
  fol->mit-scheme always-can
  run-mit-scheme always-can)

(define-backend floating-mit-scheme
  "Like mit-scheme, but force floating-point arithmetic"
  fol->floating-mit-scheme always-can
  run-mit-scheme always-can)

(define-backend standalone-mit-scheme
  "Like mit-scheme, but include FOL runtime"
  fol->standalone-mit-scheme always-can
  run-standalone-mit-scheme (needs-executable "mit-scheme"))

(define-backend stalin
  "Compile to native code via the Stalin Scheme compiler"
  fol->stalin (needs-executable "stalin")
  #f (never-can "cannot execute stalin output")) ; TODO

(define-backend common-lisp
  "Generate Common Lisp and compile to native code via SBCL"
  fol->common-lisp (needs-executable "sbcl")
  run-common-lisp (needs-executable "sbcl"))
