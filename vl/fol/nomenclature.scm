(declare (usual-integrations))

(define *symbol-count* 0)

(define-structure
  (fol-name
   safe-accessors
   (print-procedure
    (lambda (state object)
      (with-current-unparser-state state
        (lambda (port)
          (write (fol-name-base object) port)
          (write '- port)
          (write (fol-name-count object) port))))))
  base
  count)

(define (name-base thing)
  (cond ((fol-name? thing)
         (fol-name-base thing))
        ((symbol? thing) thing)
        (else (error "Invalid name" thing))))

(define (make-name template)
  (set! *symbol-count* (+ *symbol-count* 1))
  (make-fol-name (name-base template) *symbol-count*))

(define (fol-var? thing)
  (or (symbol? thing)
      (fol-name? thing)))

(define (name->symbol thing)
  (cond ((fol-name? thing)
         (symbol (fol-name-base thing) '- (fol-name-count thing)))
        ((symbol? thing) thing)
        (else "Invalid var" thing)))

(define prepare-for-scheme
  (rule-simplifier
   (list
    (rule `(? name ,fol-name?) (name->symbol name)))))

