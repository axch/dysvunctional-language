(declare (usual-integrations))

(define-structure
  (closure
   safe-accessors
   (constructor %make-closure)
   (print-procedure
    (simple-unparser-method 'closure
     (lambda (closure)
       (list (closure-exp closure)
             (closure-env closure))))))
  exp
  env)

(define (closure-formal closure)
  (lambda-formal (closure-exp closure)))

(define (closure-body closure)
  (lambda-body (closure-exp closure)))

(define (env-slice env variables)
  (make-env
   (filter (lambda (binding)
             (member (car binding) variables))
           (env-bindings env))))

;;; To keep environments in canonical form, closures only keep the
;;; variables they want.
(define (make-closure exp env)
  (let ((free (free-variables exp)))
    (%make-closure exp (env-slice env free))))

(define (closure-free-variables closure)
  (free-variables (closure-exp closure)))

(define (object-map f object)
  (cond ((closure? object)
         (make-closure (closure-exp object) (f (closure-env object))))
        ((env? object)
         (env-map f object))
        ((pair? object)
         (cons (f (car object)) (f (cdr object))))
        (else
         object)))

(define (congruent-map f object1 object2 lose)
  (cond ((and (closure? object1) (closure? object2)
              (equal? (closure-exp object1) (closure-exp object2)))
         (make-closure
          (closure-exp object1)
          (f (closure-env object1) (closure-env object2))))
        ((and (env? object1) (env? object2))
         (congruent-env-map f object1 object2 lose))
        ((and (pair? object1) (pair? object2))
         (cons (f (car object1) (car object2))
               (f (cdr object1) (cdr object2))))
        (else
         (lose))))

(define (object-reduce reducer object)
  (cond ((closure? object)
         (reducer (list (closure-env object))))
        ((env? object)
         (reducer (map cdr (env-bindings object))))
        ((pair? object)
         (reducer (list (car object) (cdr object))))
        (else
         (reducer '()))))

(define (congruent-reduce reducer object1 object2 lose)
  (cond ((and (closure? object1) (closure? object2)
              (equal? (closure-exp object1) (closure-exp object2)))
         (reducer (list (closure-env object1))
                  (list (closure-env object2))))
        ((and (env? object1) (env? object2)
              (equal? (map car (env-bindings object1))
                      (map car (env-bindings object2))))
         (reducer (map cdr (env-bindings object1))
                  (map cdr (env-bindings object2))))
        ((and (pair? object1) (pair? object2))
         (reducer (list (car object1) (cdr object1))
                  (list (car object2) (cdr object2))))
        (else
         (lose))))

;;; Manually specializing the two-list binary case (since the MIT
;;; Scheme compiler doesn't do it for me).
(define every
  (let ((every (access every (->environment '(runtime srfi-1)))))
    (lambda (pred lis1 . lists)
      (if (and (pair? lists) (null? (cdr lists)))
          (or (null? lis1) (null? (car lists))
              (let loop ((head1 (car lis1)) (tail1 (cdr lis1))
                         (head2 (car (car lists))) (tail2 (cdr (car lists))))
                (if (or (null? tail1) (null? tail2))
                    (pred head1 head2)
                    (and (pred head1 head2)
                         (loop (car tail1) (cdr tail1)
                               (car tail2) (cdr tail2))))))
          (apply every pred lis1 lists)))))

(define (memoize cache f)
  (lambda (x)
    (hash-table/lookup cache x
     (lambda (datum) datum)
     (lambda ()
       (let ((answer (f x)))
         (hash-table/put! cache x answer)
         answer)))))

(define free-variables
  (memoize (make-eq-hash-table)
   (lambda (form)
     (cond ((constant? form)
            '())
           ((variable? form)
            (list form))
           ((pair-form? form)
            (lset-union equal? (free-variables (car-subform form))
                        (free-variables (cdr-subform form))))
           ((lambda-form? form)
            (lset-difference equal? (free-variables (lambda-body form))
                             (free-variables (lambda-formal form))))
           ((pair? form)
            (lset-union equal? (free-variables (car form))
                        (free-variables (cdr form))))
           (else
            (error "Invalid expression type" form))))))

(define-structure
  (world
   safe-accessors
   (print-procedure
    (simple-unparser-method 'world
     (lambda (world)
       (list (world-gensym world))))))
  gensym)

(define (initial-world)
  (make-world 0))

(define impossible-world (make-world #f))
(define (impossible-world? thing)
  (eq? thing impossible-world))

(define (world-equal? world1 world2)
  (equal? (world-gensym world1) (world-gensym world2)))

(define-structure
  (gensym
   safe-accessors
   (print-procedure
    (simple-unparser-method 'gensym
     (lambda (gensym)
       (list (gensym-number gensym))))))
  number)

(define (union-world world1 world2)
  (cond ((impossible-world? world1) world2)
        ((impossible-world? world2) world1)
        (else
         (make-world
          (max (world-gensym world1) (world-gensym world2))))))

(define (current-gensym world)
  (make-gensym (world-gensym world)))

(define (do-gensym world)
  (make-world
   (+ 1 (world-gensym world))))
