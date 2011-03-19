(declare (usual-integrations))
;;;; Alias elimination

(define (de-alias-expression expr env)
  ;; An alias environment is not like a normal environment.  This
  ;; environment maps every bound name to whether it is an alias or
  ;; not; the latter case is represented by binding the variable to
  ;; itself.  It is important to know when such an environment does
  ;; not bind a variable at all; that means that variable in not in
  ;; scope here.  For purposes of this process, (constant) numbers,
  ;; booleans, and empty-lists are legitimate things that variables
  ;; may be aliases of.
  (define (augment-env env old-names aliases win)
    (define (acceptable-alias? alias)
      (and (not (non-alias? alias))
           (or (number? alias)
               (boolean? alias)
               (null? alias)
               (lookup alias env))))
    (let ((aliases (if (non-alias? aliases)
                       (make-list (length old-names) the-non-alias)
                       aliases)))
      (win
       (append
        (map (lambda (old-name alias)
               (if (acceptable-alias? alias)
                   (cons old-name alias)
                   (cons old-name old-name)))
             old-names
             aliases)
        env)
       (map acceptable-alias? aliases))))
  (define lookup assq)
  (define the-non-alias (list 'not-an-alias))
  (define (non-alias? thing)
    (eq? the-non-alias thing))
  (define (merge-name-lists names1 names2)
    (if (or (non-alias? names1) (non-alias? names2))
        the-non-alias
        (map (lambda (name1 name2)
               (if (eq? name1 name2)
                   name1
                   the-non-alias))
             names1 names2)))
  ;; The win continuation accepts the new, de-aliased expression, and
  ;; a list of the names of the variables that hold the return values
  ;; from this expression.
  (define (loop expr env win)
    (cond ((symbol? expr)
           (let ((alias-binding (lookup expr env)))
             (if alias-binding
                 (win (cdr alias-binding) (list (cdr alias-binding)))
                 (error "Trying to de-alias an unbound variable" expr env))))
          ((number? expr)
           (win expr (list expr)))
          ((boolean? expr)
           (win expr (list expr)))
          ((null? expr)
           (win expr (list expr)))
          ((values-form? expr)
           (loop* (cdr expr) env
            (lambda (exprs names-lists)
              (win `(values ,@exprs) (apply append names-lists)))))
          ((if-form? expr)
           (loop (cadr expr) env
            (lambda (new-pred pred-names)
              ;; TODO Pred-shape better be a boolean
              (loop (caddr expr) env
               (lambda (new-cons cons-names)
                 (loop (cadddr expr) env
                  (lambda (new-alt alt-names)
                    (win `(if ,new-pred ,new-cons ,new-alt)
                         (merge-name-lists cons-names alt-names)))))))))
          ((let-form? expr)
           (let ((bindings (cadr expr))
                 (body (caddr expr)))
             (if (null? (cdddr expr))
                 (loop* (map cadr bindings) env
                  (lambda (new-bind-expressions bind-name-lists)
                    (let ((bind-names
                           (map (lambda (bind-name-list)
                                  ;; These better all be singletons
                                  (if (non-alias? bind-name-list)
                                      the-non-alias
                                      (car bind-name-list)))
                                bind-name-lists)))
                      (augment-env env (map car bindings) bind-names
                       (lambda (env acceptable-aliases)
                         (loop body env
                          (lambda (new-body body-name-list)
                            (win (empty-let-rule
                                  `(let ,(filter-map
                                          (lambda (name alias? expr)
                                            (and (not alias?)
                                                 (list name expr)))
                                          (map car bindings)
                                          acceptable-aliases
                                          new-bind-expressions)
                                     ,new-body))
                                 body-name-list))))))))
                 (error "Malformed LET" expr))))
          ((let-values-form? expr)
           (let* ((binding (caadr expr))
                  (names (car binding))
                  (subexpr (cadr binding))
                  (body (caddr expr)))
             (if (null? (cdddr expr))
                 (loop subexpr env
                  (lambda (new-subexpr subexpr-names)
                    (augment-env env names subexpr-names
                     (lambda (env acceptable-aliases)
                       (loop body env
                        (lambda (new-body body-name-list)
                          ;; ACCEPTABLE-ALIASES tells me which of
                          ;; these bindings are guaranteed to be dead
                          ;; because the variables being bound are
                          ;; aliases and have already been replaced in
                          ;; the new body.  I could eliminate them,
                          ;; but that would require traversing subexpr
                          ;; again to look for the VALUES that
                          ;; supplies the corresponding values.  For
                          ;; now, I will just kill the whole
                          ;; let-values if it is useless.
                          (win (if (any not acceptable-aliases)
                                   `(let-values ((,names ,new-subexpr))
                                      ,new-body)
                                   new-body)
                               body-name-list)))))))
                 (error "Malformed LET-VALUES" expr))))
          (else ;; general application
           (loop* (cdr expr) env
            (lambda (new-args args-names-lists)
              (win `(,(car expr) ,@new-args)
                   the-non-alias))))))
  (define (loop* exprs env win)
    (if (null? exprs)
        (win '() '())
        (loop (car exprs) env
         (lambda (new-expr expr-names)
           (loop* (cdr exprs) env
            (lambda (new-exprs expr-names-lists)
              (win (cons new-expr new-exprs)
                   (cons expr-names expr-names-lists))))))))
  (loop expr env (lambda (new-expr name-list)
                   ;; The name list might be useful to an
                   ;; interprocedural must-alias crunch.
                   new-expr)))

(define (intraprocedural-de-alias program)
  (if (begin-form? program)
      (append
       (map
        (rule `(define ((? name ,symbol?) (?? formals))
                 (argument-types (?? stuff))
                 (? body))
              `(define (,name ,@formals)
                 (argument-types ,@stuff)
                 ,(de-alias-expression body (map cons formals formals))))
        (except-last-pair program))
       (list (de-alias-expression (car (last-pair program)) '())))
      (de-alias-expression program '())))

;;; I don't (yet) even want to think about what it would take to do a
;;; good job of interprocedural de-aliasing.
