(declare (usual-integrations))
;;;; Compilation with Stalin

(define (read-source file)
  (with-input-from-file file
    (lambda ()
      (let loop ((forms '()))
        (let ((form (read)))
          (if (eof-object? form)
              (reverse forms)
              (loop (cons form forms))))))))

(define (fol->stalin program #!optional output-base)
  (if (default-object? output-base)
      (set! output-base "stanozzle"))
  (let* ((runtime (read-source (string-append my-path "runtime.sc")))
         (output (append runtime (prepare-for-stalin program)))
         (output-file (pathname-new-type output-base "sc")))
    (with-output-to-file output-file
      (lambda ()
        (for-each (lambda (form)
                    (pp form)
                    (newline))
                  output)))
    (run-shell-command (string-append "stalin -On -d -q " (->namestring output-file)))))

(define replace-let-values
  (rule-simplifier
   (list
    (rule `(let-values (((? names) (? exp)))
             (?? body))
          (let ((the-name (make-name 'the-values)))
            `(let ((,the-name ,exp))
               (let ,(map (lambda (name index)
                            `(,name (vector-ref ,the-name ,index)))
                          names
                          (iota (length names)))
                 ,@body)))))))

(define replace-nulls
  (on-subexpressions
   (rule '()
          '(quote ()))))

(define (strip-begin program)
  (if (begin-form? program)
      (cdr program)
      program))

(define (write-last-value program)
  `(,@(except-last-pair program)
    (write ,(last program))))

(define (prepare-for-stalin program)
  (write-last-value
   (replace-nulls
    (replace-let-values
     (strip-begin
      (strip-argument-types
       program))))))
