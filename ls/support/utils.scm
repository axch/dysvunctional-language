(declare (usual-integrations))

(define (hash-table/put-alist! table alist)
  (for-each (lambda (k.v)
              (hash-table/put! table (car k.v) (cdr k.v)))
            alist))

(define (alist->eq-hash-table alist)
  (let ((answer (make-eq-hash-table)))
    (hash-table/put-alist! answer alist)
    answer))

;; TODO Backwards compatibility:
(define hash-table-types-available?
  (not (lexical-unbound? (the-environment) 'hash-table-entry-type:strong)))

(define make-ontology
  (if hash-table-types-available?
      make-hash-table-type
      list))

(define (ontology-key=? ontology)
  ((if ((access hash-table-type? (->environment '(runtime hash-table))) ontology)
       (access table-type-key=? (->environment '(runtime hash-table)))
       cadr) ontology))

(define ((unique hash-table-type) lst)
  ;; This hash-table-type is really meant to be an ontology of what
  ;; sorts of operations are available for these objects, but
  ;; hash-table-types will serve my purpose for now.
  (let ((len (length lst)))
    (if (and (> len 10)                    ; How to set this parameter?
             ((access hash-table-type? (->environment '(runtime hash-table)))
              hash-table-type))
        ;; The hash-table method is linear over delete-duplicates'
        ;; quadratic, but the constant factors are appreciably worse.
        (let ((table (((access hash-table-constructor (->environment '(runtime hash-table)))
                       ;; TODO Is there really no exported way to make a
                       ;; hash table of a given type?
                       hash-table-type) len)))
          (define (keep? elt)
            (if (hash-table/get table elt #f)
                #f
                (begin
                  (hash-table/put! table elt #t)
                  #t)))
          (filter keep? lst))
        (delete-duplicates lst (ontology-key=? hash-table-type)))))

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
    (if (eq? name 'generate-stage)
        ;; The generate stage wants to display different stats
        (let ((analysis (property-value 'analysis input)))
          (display "Stage generate on ")
          (display
           (length
            ((access analysis-bindings user-initial-environment) ;; TODO I need a real module system!
             analysis)))
          (display " bindings"))
        (let ((size (count-pairs input))
              (stripped-size (count-pairs (strip-argument-types input))))
          (display "Stage ")
          (display name)
          (display " on ")
          (display stripped-size)
          (display " pairs + ")
          (display (- size stripped-size))
          (display " pairs of type annotations")))
    (newline)
    (flush-output)
    (let ((answer (show-time (lambda () (stage input)))))
      (newline)
      answer)))

(define (report-size program)
  (let ((size (count-pairs program))
        (stripped-size (count-pairs (strip-argument-types program))))
      (display "Final output has ")
      (display stripped-size)
      (display " pairs + ")
      (display (- size stripped-size))
      (display " pairs of type annotations")
      (newline))
  program)

(define (force-assq key lst)
  (let ((binding (assq key lst)))
    (if binding
        (cdr binding)
        (error "Did not find" key lst))))

(define (fmap-maybe f object)
  (if object (f object) #f))
