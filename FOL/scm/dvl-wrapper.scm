(set! load/suppress-loading-message? #t)

(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-relative "../../ls/dvl/load")
(pp (compile-to-raw-fol (read)))
(%exit 0)
