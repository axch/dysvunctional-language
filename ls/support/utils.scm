(declare (usual-integrations))

(define (hash-table/put-alist! table alist)
  (for-each (lambda (k.v)
              (hash-table/put! table (car k.v) (cdr k.v)))
            alist))

(define (alist->eq-hash-table alist)
  (let ((answer (make-eq-hash-table)))
    (hash-table/put-alist! answer alist)
    answer))

;; This is useful for inspecting the outputs of various compiler
;; stages.
(define (count-pairs thing)
  (if (pair? thing)
      (+ 1 (count-pairs (car thing))
         (count-pairs (cdr thing)))
      0))

(define (occurs-in-tree? thing tree)
  (cond ((equal? thing tree) #t)
        ((pair? tree)
         (or (occurs-in-tree? thing (car tree))
             (occurs-in-tree? thing (cdr tree))))
        (else #f)))

(define (replace-in-tree thing new tree)
  (cond ((equal? thing tree) new)
        ((pair? tree)
         (cons (replace-in-tree thing new (car tree))
               (replace-in-tree thing new (cdr tree))))
        (else tree)))

;; filter-map-tree :: (a -> b) x (cons-tree a) -> [b]
;; filter-map-tree does not preserve the tree structure of the input.
 (define (filter-map-tree proc tree)
  (let walk ((tree tree) (answer '()))
    (if (pair? tree)
        (walk (car tree) (walk (cdr tree) answer))
        (let ((elt (proc tree)))
          (if elt
              (cons elt answer)
              answer)))))

(define (assert pred)
  (if (not pred)
      (error "Assertion failed")))

(define-syntax visible-stage
  (syntax-rules ()
    ((_ name)
     (visible-named-stage name 'name))))

(define (visible-named-stage stage name)
  (lambda (input)
    (display "Stage ")
    (display name)
    (display " on ")
    (display (count-pairs input))
    (display " pairs")
    (newline)
    (let ((answer (show-time (lambda () (stage input)))))
      (newline)
      answer)))

(define (report-size program)
  (display "Final output has ")
  (display (count-pairs program))
  (display " pairs")
  (newline)
  program)
