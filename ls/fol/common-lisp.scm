(declare (usual-integrations))
;;;; Simplistic FOL to Common Lisp compiler.

(define (compile-program program)
  (define compile-definition
    (rule `(define ((? name ,fol-var?) (?? formals))
             (argument-types (?? stuff))
             (? body))
          `(defun ,name (,@formals)
             ,(compile-expression body))))
  (if (begin-form? program)
      `(progn
        ,@prelude
        ,@(map compile-definition
               (cdr (except-last-pair program)))
        ,(compile-expression (last program)))
      (compile-expression program)))

(define prelude
  '((declaim (inline zero?
                     positive?
                     negative?
                     read-real
                     write-real
                     real               ; TODO REAL is a locked name
                     gensym=))
    (defun zero? (x)
      (zerop x))
    (defun positive? (x)
      (plusp x))
    (defun negative? (x)
      (minusp x))
    (defun read-real ()
      (read))
    (defun write-real (x)
      (format t "~F~%" x))
    (defun real (x)
      x)
    (defun gensym= (gensym1 gensym2)
      (eq gensym1 gensym2))))

(define (compile-expression expr)
  (cond ((fol-var? expr)
         expr)
        ((fol-const? expr)
         (compile-const expr))
        ((if-form? expr)
         (compile-if expr))
        ((let-form? expr)
         (compile-let expr))
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
         expr)
        ((boolean? expr)
         (if expr 't 'nil))
        (else
         'nil)))

(define compile-if
  (rule `(if (? pred)
             (? cons)
             (? alt))
        `(if ,(compile-expression pred)
             ,(compile-expression cons)
             ,(compile-expression alt))))

(define compile-let
  (rule `(let ((?? bindings))
           (? body))
        `(let (,@(map (lambda (binding)
                        `(,(car binding) ,(compile-expression (cadr binding))))
                      bindings))
           ,(compile-expression body))))

(define compile-let-values
  (rule `(let-values ((?? names) (? expr))
           (? body))
        `(muliple-values-bind (,@names) ,(compile-expression expr)
           ,(compile-expression body))))

(define compile-lambda
  (rule `(lambda ((? var))
           (? body))
        `(function
          (lambda (,var)
            ,(compile-expression body)))))

(define compile-vector-ref
  (rule `(vector-ref (? expr) (? index))
        `(svref ,(compile-expression expr) ,index)))

(define vector-ref-form? (tagged-list? 'vector-ref))

(define (compile-application expr)
  (map compile-expression expr))
