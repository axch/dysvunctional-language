(define (self-relatively thunk)
  (let ((place (ignore-errors current-load-pathname)))
    (if (pathname? place)
	(with-working-directory-pathname
	 (directory-namestring place)
	 thunk)
	(thunk))))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-relative "../vl/support/auto-compilation")

(load-relative-compiled "data")
(load-relative-compiled "read")
(load-relative-compiled "macro")
(load-relative-compiled "letrec")
(load-relative-compiled "env")
(load-relative-compiled "eval")
(load-relative-compiled "forward-mode")
(load-relative-compiled "primitives")
