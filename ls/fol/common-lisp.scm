(declare (usual-integrations))
;;;; Simplistic FOL to Common Lisp compiler.

(define (fol->common-lisp program #!optional base)
  (if (default-object? base)
      (set! base "comozzle"))
  (let ((code (prepare-for-common-lisp program))
        (file (pathname-new-type base "lisp")))
    (with-output-to-file file
      (lambda ()
        (fluid-let ((flonum-unparser-cutoff '(normal 0 scientific)))
          (for-each pp code))))
    (run-shell-command
     (format #f
      "sbcl --eval '(progn (compile-file ~S :verbose t :print t) (quit))'"
      (->namestring file)))))

(define (prepare-for-common-lisp program)
  (define (force-values thing)
    (if (values-form? thing)
        thing
        `(values ,thing)))
  (define (compile-program program)
    (let ((inferred-type-map (make-eq-hash-table)))
      (check-program-types program inferred-type-map)
      (define (lookup-inferred-type expr)
        (or (hash-table/get inferred-type-map expr #f)
            (error "Looking up unknown expression" expr)))
      (define compile-definition
        (rule `(define ((? name ,fol-var?) (?? formals))
                 (argument-types (?? formal-types) (? return-type))
                 (? body))
              `(,name (,@formals)
                 (declare ,@(map (lambda (formal-type formal)
                                   `(type ,(fol-shape->type-specifier formal-type)
                                          ,formal))
                                 formal-types
                                 formals))
                 (declare ,(force-values (fol-shape->type-specifier return-type)))
                 ,(compile-expression body lookup-inferred-type))))
      (define (compile-entry-point expression)
        `(__main__ ()
           ,(compile-expression expression lookup-inferred-type)))
      ;; TODO This let->let* is a bug waiting to happen, because it
      ;; invalidates most of the inferred-type-map constructed above.
      ;; It's OK in the current setup because that type map is only
      ;; used for procedure call expressions, which after let-lifting
      ;; will not have any let expressions as subexpressions, and
      ;; therefore will not, in fact, be changed by let->let*.  There
      ;; are two ways to fix this disaster: either extend the type
      ;; checker to handle let* forms and do the conversion before
      ;; computing the inferred type map, or extend the type
      ;; declaration facilities enough that the inferred type map is
      ;; not necessary and all the type information this translation
      ;; needs can be read off the input FOL program.
      (let ((program (let->let* program)))
        `((declaim (optimize (speed 3) (safety 0)))
          ,@prelude
          (labels (,@(if (begin-form? program)
                         `(,@(map compile-definition
                                  (cdr (except-last-pair program)))
                           ,(compile-entry-point (last program)))
                         (list (compile-entry-point program))))
                  (setf (fdefinition '__main__) (function __main__)))))))
  (compile-program (alpha-rename program)))

(define *fol->cl-desired-precision* 'double-float)

(define (fol-shape->type-specifier shape)
  (cond ((eq? 'real shape)
         *fol->cl-desired-precision*)
        ((eq? 'bool shape)
         'boolean)
        ((null? shape)
         'null)
        ((eq? shape 'escaping-function)
         'function)
        ((eq? 'cons (car shape))
         `(cons ,(fol-shape->type-specifier (cadr  shape))
                ,(fol-shape->type-specifier (caddr shape))))
        ;; Heterogeneous vector types are not supported by CL.
        ((eq? 'vector (car shape))
         `(simple-vector ,(length (cdr shape))))
        ((eq? 'values (car shape))
         `(values ,@(map fol-shape->type-specifier (cdr shape))))
        (else
         (error "Bogus shape " shape))))

(define prelude
  `((declaim (inline zero?
                     positive?
                     negative?
                     read-real
                     write-real
                     gensym=))
    (defun zero? (x)
      (declare (type ,*fol->cl-desired-precision* x))
      (zerop x))
    (defun positive? (x)
      (declare (type ,*fol->cl-desired-precision* x))
      (plusp x))
    (defun negative? (x)
      (declare (type ,*fol->cl-desired-precision* x))
      (minusp x))
    (defun read-real ()
      (read))
    (defun write-real (x)
      (declare (type ,*fol->cl-desired-precision* x))
      (format t "~F~%" x)
      x)
    (defun gensym= (gensym1 gensym2)
      (declare (type symbol gensym1 gensym2))
      (eq gensym1 gensym2))))

(define (compile-expression expr lookup-inferred-type)
  (define (%compile-expression expr)
    (loop expr))
  (define (loop expr)
    (cond ((fol-var? expr) expr)
          ((fol-const? expr)
           (compile-const expr))
          ((if-form? expr)
           (compile-if expr))
          ((let-form? expr)
           (compile-let expr))
          ((let*-form? expr)
           (compile-let* expr))
          ((let-values-form? expr)
           (compile-let-values expr))
          ((lambda-form? expr)
           (compile-lambda expr))
          ((vector-ref-form? expr)
           (compile-vector-ref expr))
          (else
           (compile-application expr))))
  (define (compile-const expr)
    (cond ((number? expr)
           `(coerce ,(if (exact? expr) (exact->inexact expr) expr) ',*fol->cl-desired-precision*))
          ((boolean? expr)
           (if expr 't 'nil))
          (else
           'nil)))
  (define compile-if
    (rule `(if (? pred)
               (? cons)
               (? alt))
          `(if ,(%compile-expression pred)
               ,(%compile-expression cons)
               ,(%compile-expression alt))))
  (define compile-let
    (rule `(let ((?? bindings))
             (? body))
          `(let (,@(map (lambda (binding)
                          `(,(car binding)
                            ,(%compile-expression (cadr binding))))
                        bindings))
             ,(%compile-expression body))))
  (define compile-let*
    (rule `(let* ((?? bindings))
             (? body))
          `(let* (,@(map (lambda (binding)
                           `(,(car binding)
                             ,(%compile-expression (cadr binding))))
                         bindings))
             ,(%compile-expression body))))
  (define compile-let-values
    (rule `(let-values (((? names) (? expr)))
             (? body))
          `(multiple-value-bind (,@names) ,(%compile-expression expr)
             ,(%compile-expression body))))
  (define compile-lambda
    (rule `(lambda ((? var))
             (? body))
          `(function
            (lambda (,var)
              ;; TODO Support other external args than REAL
              (declare (type ,*fol->cl-desired-precision* ,var))
              ,(%compile-expression body)))))
  (define compile-vector-ref
    (rule `(vector-ref (? expr) (? index))
          `(svref ,(%compile-expression expr) ,index)))
  (define vector-ref-form? (tagged-list? 'vector-ref))
  (define (user-procedure? operator)
    (not (memq operator (append '(cons car cdr vector vector-ref)
                                (map primitive-name *primitives*)))))
  (define (compile-application expr)
    (let ((result `(,(if  (eq? 'real (car expr))
                          'identity
                          (car expr))
                    ,@(map %compile-expression (cdr expr)))))
      #;(if (user-procedure? (car expr))
          `(the ,(fol-shape->type-specifier (lookup-inferred-type expr))
                ,result)
          result)
      result))
  (%compile-expression expr))
