(declare (usual-integrations))
;;;; Alpha renaming

;;; Renaming all variables so that nothing shadows anything else is
;;; just a recursive descent on the structure of the code.  In fact,
;;; it doesn't even need to be too picky about the differences between
;;; various forms that do not bind names; it just has to catch the
;;; ones that do.

;;; The recursive traversal carries down an environment of which
;;; symbols have been renamed to which others.  If a name being bound
;;; doesn't shadow anything, it should be renamed to itself, otherwise
;;; to a fresh name.  The traversal need not bring anything up besides
;;; the renamed expression.

(define (alpha-rename program)
  ;; TODO Fix the bookkeeping of what names the primitives rely on
  (define (needed-names primitive)
    (list (primitive-name primitive)))
  (alpha-rename-exp program
   (map (lambda (name)
          (cons name name))
        (delete-duplicates
         `(cons car cdr if define let vector vector-ref let-values values
                ,@(append-map needed-names *primitives*))))))

(define (alpha-rename-exp exp env)
  (cond ((assq exp env) => cdr)
        ((lambda-form? exp)
         (let* ((names (cadr exp))
                (body (cddr exp))
                (new-env (extend-alpha-env env names))
                (new-names (map (lambda (name)
                                  (cdr (assq name new-env)))
                                names)))
           `(lambda ,new-names
              ,@(alpha-rename-exp body new-env))))
        ((let-form? exp)
         (->let (alpha-rename-exp (->lambda exp) env)))
        ((let-values-form? exp)
         (->let-values (alpha-rename-exp (->lambda exp) env)))
        ((definition? exp)
         ;; Assume the definiendum is already unique
         (reconstitute-definition
          `(define ,(definiendum exp)
             ,(alpha-rename-exp (definiens exp) env))))
        ((pair? exp)
         (cons (alpha-rename-exp (car exp) env)
               (alpha-rename-exp (cdr exp) env)))
        (else exp)))

(define (extend-alpha-env env names)
  (append
   (map cons
        names
        (map (lambda (name)
               (if (assq name env)
                   (make-name name)
                   name))
             names))
   env))

;;; With an analagous traversal, we can check whether two programs are
;;; alpha-renamings of each other.

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
          ((and (definition? exp1) (definition? exp2))
           (let ((name1 (definiendum exp1))
                 (name2 (definiendum exp2)))
             (loop (definiens exp1) (definiens exp2) (cons (cons name1 name2) env))))
          ((and (pair? exp1) (pair? exp2))
           (and (loop (car exp1) (car exp2) env)
                (loop (cdr exp1) (cdr exp2) env)))
          (else (equal? exp1 exp2)))))

;;; Also, we can check whether a program is already alpha renamed
;;; (i.e. already contains no shadowing) by looking to see whether it
;;; alpha renames to itself.

(define (alpha-renamed? program)
  (equal? program (alpha-rename program)))
