(define (argmin f x0)
  (let loop ((x x0))
    (let ((grad ((gradient f) x)))
      (if (small? grad)
          x
          (let ((line (lambda (d) (+ x (* d grad)))))
            (loop (line (line-search (compose f line)))))))))

(define (line-search f)
  ;; For example, one step of Newton's method, involves first and
  ;; second derivatives of f.
  )

;; The optimization routine is generic
;; The line search routine is generic
;; They are independent
;; Etc.
