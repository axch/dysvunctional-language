;; This is iterate, specialized to f being (step c) and x having shape
;; (R . R)
(define (operation-231 self-env count f-env x)
   (type env-226 real env-235 (cons real real)
         (cons real real))
   (if (<= count 0)
       x
       ;; It calls itself because the arguments to the recursive call
       ;; have the same shapes
       (operation-231 self-env
                      (- count 1)
                      f-env
                      (operation-23 f-env x))))

;; This is the (step c) lambda where c is (R . R) and
;; the argument is (R . R)
(define (operation-23 self-env z)
  (type env-235 (cons real real)
        (cons real real))
  (operation-423 ; Complex addition
   (env-235-c:+-env self-env)
   (operation-249 ; Complex multiplication
    (env-235-c:*-env self-env) z z)
   (env-235-c self-env)))
