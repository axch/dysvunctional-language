(declare (usual-integrations))
;;;; Alpha renaming

;;; Renaming all variables so that all bound names are unique is
;;; just a recursive descent on the structure of the code.  In fact,
;;; it doesn't even need to be too picky about the differences between
;;; various forms that do not bind names; it just has to catch the
;;; ones that do.

;;; The recursive traversal carries down an environment of which
;;; symbols currently in scope have been renamed to which others, and
;;; also a list of symbols that are out of scope now but have been
;;; bound elsewhere in the program.  If a name being bound has not
;;; been bound before, it should be renamed to itself, otherwise to a
;;; fresh name.  In any case, it should be registered as a name that
;;; has been bound at some point.  In addition to the renamed
;;; expression, the traversal needs to return the set of names that
;;; expression had bound, so that other bindings elsewhere can avoid
;;; them.  This global uniqueness is necessary because various later
;;; operations may change the scopes of bound names.

(define (alpha-rename program)
  (alpha-rename-exp program
   (map (lambda (name)
          (cons name name))
        (append fol-reserved (map primitive-name *primitives*)))
   '()
   (lambda (new-program new-env) new-program)))

;;; This is written in continuation passing style because the
;;; traversal needs to return two things.
(define (alpha-rename-exp exp env avoid win)
  (cond ((assq exp env)
         (win (cdr (assq exp env)) avoid))
        ((lambda-form? exp)
         (let* ((names (cadr exp))
                (body (cddr exp))
                (new-env (extend-alpha-env env names avoid))
                (new-names (map (lambda (name)
                                  (cdr (assq name new-env)))
                                names)))
           (alpha-rename-exp body new-env avoid
            (lambda (new-body body-avoid)
              (win `(lambda ,new-names ,@new-body)
                   ;; Technically only need those names that are not
                   ;; still in scope outside the lambda.
                   (lset-union eq? body-avoid names))))))
        ((let-form? exp)
         (alpha-rename-exp (->lambda exp) env avoid
          (lambda (new-exp new-avoid)
            (win (->let new-exp) new-avoid))))
        ((let-values-form? exp)
         (alpha-rename-exp (->lambda exp) env avoid
          (lambda (new-exp new-avoid)
            (win (->let-values new-exp) new-avoid))))
        ((definition? exp)
         ;; Assume the definiendum is already unique
         (alpha-rename-exp (definiens exp) env avoid
          (lambda (new-definiens new-avoid)
            (win (reconstitute-definition
                  `(define ,(definiendum exp)
                     ,new-definiens))
                 new-avoid))))
        ((pair? exp)
         (alpha-rename-exp (car exp) env avoid
          (lambda (new-car car-avoid)
            (alpha-rename-exp (cdr exp) env car-avoid
             (lambda (new-cdr cdr-avoid)
               (win (cons new-car new-cdr) cdr-avoid))))))
        (else (win exp avoid))))

(define (extend-alpha-env env names avoid)
  (append
   (map cons
        names
        (map (lambda (name)
               (if (or (assq name env) (memq name avoid))
                   (make-name name)
                   name))
             names))
   env))

;;; The traversal to check whether two programs are alpha-renamings of
;;; each other is much simpler, because there is no reason to carry
;;; around any avoid list, and because the recursive call need only
;;; return one thing.

(define (alpha-rename? exp1 exp2)
  (let loop ((exp1 exp1)
             (exp2 exp2)
             (env '()))
    (cond ((assq exp1 env) => (lambda (bind) (equal? (cdr bind) exp2)))
          ((and (lambda-form? exp1) (lambda-form? exp2))
           (let* ((names1 (cadr exp1))
                  (body1 (cddr exp1))
                  (names2 (cadr exp2))
                  (body2 (cddr exp2))
                  (new-env (append (map cons names1 names2) env)))
             (loop body1 body2 new-env)))
          ((and (let-form? exp1) (let-form? exp2))
           (loop (->lambda exp1) (->lambda exp2) env))
          ((and (let-values-form? exp1) (let-values-form? exp2))
           (loop (->lambda exp1) (->lambda exp2) env))
          ;; Do I want to mess with the fact that the order of
          ;; definitions is semantically insignificant?
          ((and (begin-form? exp1) (begin-form? exp2))
           (let* ((names1 (map definiendum (filter definition? exp1)))
                  (names2 (map definiendum (filter definition? exp2)))
                  (new-env (append (map cons names1 names2) env)))
             (apply boolean/and
              (map (lambda (form1 form2)
                     (loop form1 form2 new-env))
                   exp1 exp2))))
          ;; At this point, the environment has already accounted
          ;; for the scope of the definitions
          ((and (definition? exp1) (definition? exp2))
           (let ((name1 (definiendum exp1))
                 (name2 (definiendum exp2)))
             (and (loop name1 name2 env)
                  (loop (definiens exp1) (definiens exp2) env))))
          ((and (pair? exp1) (pair? exp2))
           (and (loop (car exp1) (car exp2) env)
                (loop (cdr exp1) (cdr exp2) env)))
          (else (equal? exp1 exp2)))))

;;; Also, we can check whether a program is already alpha renamed
;;; (i.e. already has unique names) by looking to see whether it alpha
;;; renames to itself.

(define (unique-names? program)
  (equal? program (alpha-rename program)))
