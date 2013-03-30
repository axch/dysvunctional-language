(declare (usual-integrations))
(declare (integrate-external "syntax"))
(declare (integrate-external "../support/pattern-matching"))
;;;; Simplistic FOL to Common Lisp compiler.

(define (fol->common-lisp program #!optional base)
  (if (default-object? base)
      (set! base "comozzle"))
  (let ((code `((declaim (optimize (speed 3) (safety 0)))
                ,@prelude
                ,@(prepare-for-common-lisp program)))
        (file (pathname-new-type base "lisp")))
    (with-output-to-file file
      (lambda ()
        (fluid-let ((flonum-unparser-cutoff '(normal 0 scientific)))
          (for-each pp code))))
    (run-shell-command
     (format #f
      "sbcl --eval '(progn (compile-file ~S :verbose t :print t) (quit))'"
      (->namestring file)))))

(define (run-common-lisp #!optional base)
  (if (default-object? base)
      (set! base "comozzle"))
  (let ((file (pathname-new-type base "fasl")))
    (run-shell-command
     (format #f
      "sbcl --noinform --eval '(progn (load ~S) (write (__main__)) (quit))'"
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
      (define compile-type-definition
        (rule `(define-type (? name ,fol-var?) (structure (?? slots-and-types)))
              (let ((slots (map car slots-and-types))
                    (types (map cadr slots-and-types)))
                (define (slot-declaration slot type)
                  ;; According to the Hyperspec, nil is not a type
                  ;; error unless some construction attempt omits the
                  ;; initialization parameter.
                  `(,slot nil :type ,(fol-shape->type-specifier type) :read-only t))
                `(defstruct (,name (:constructor ,(symbol 'make- name) ,slots))
                   ,@(map slot-declaration slots types)))))
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
      (let* ((program (let->let* program))
             (types (if (begin-form? program)
                        (filter type-definition? program)
                        '()))
             (code (if (begin-form? program)
                       (filter (lambda (d) (not (type-definition? d))) (cdr program))
                       (list program))))
        `(,@(map compile-type-definition types)
          (labels (,@(map compile-definition (except-last-pair code))
                   ,(compile-entry-point (last code)))
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
        ((fol-var? shape)
         ;; Assume it's a user-defined struct
         shape)
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
  (define-case* loop
    ((fol-var expr) expr)
    ((number expr)
     `(coerce
       ,(if (exact? expr) (exact->inexact expr) expr)
       ',*fol->cl-desired-precision*))
    ((boolean expr) (if expr 't 'nil))
    ((null) 'nil)
    (if-form => compile-if)
    (let-form => compile-let)
    (let*-form => compile-let*)
    (let-values-form => compile-let-values)
    (lambda-form => compile-lambda)
    (vector-ref-form => compile-vector-ref)
    (pair => compile-application))
  (define (compile-if pred cons alt)
    `(if ,(%compile-expression pred)
         ,(%compile-expression cons)
         ,(%compile-expression alt)))
  (define (compile-let bindings body)
    `(let (,@(map (lambda (binding)
                    `(,(car binding)
                      ,(%compile-expression (cadr binding))))
                  bindings))
       ,(%compile-expression body)))
  (define (compile-let* bindings body)
    `(let* (,@(map (lambda (binding)
                     `(,(car binding)
                       ,(%compile-expression (cadr binding))))
                   bindings))
       ,(%compile-expression body)))
  (define (compile-let-values names expr body)
    `(multiple-value-bind (,@names) ,(%compile-expression expr)
                          ,(%compile-expression body)))
  (define (compile-lambda formals body)
    `(function
      (lambda (,@formals)
        ;; TODO Support other external args than REAL, and maybe more than one
        (declare (type ,*fol->cl-desired-precision* ,@formals))
        ,(%compile-expression body))))
  (define (compile-vector-ref expr index)
    `(svref ,(%compile-expression expr) ,index))
  (define vector-ref-form? (tagged-list? 'vector-ref))
  (define-algebraic-matcher vector-ref-form vector-ref-form? cadr caddr)
  (define (user-procedure? operator)
    (not (memq operator (append '(cons car cdr vector vector-ref)
                                (map primitive-name *primitives*)))))
  (define (compile-application operator operands)
    `(,(if (eq? 'real operator)
           'identity
           operator)
      ,@(map %compile-expression operands)))
  (%compile-expression expr))
