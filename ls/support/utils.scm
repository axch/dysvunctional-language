(declare (usual-integrations))

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

(define (record-fields record)
  (let ((type (record-type-descriptor record)))
    (map (lambda (field-name)
           ((record-accessor type field-name) record))
         (record-type-field-names type))))

(define (estimate-space-usage thing)
  (define (sum lst) (apply + lst))
  (define seen-table (make-strong-eq-hash-table))
  (define (seen? thing)
    (hash-table/get seen-table thing #f))
  (define (seen! thing)
    (hash-table/put! seen-table thing #t))
  (let loop ((thing thing))
    (cond ((seen? thing) 1) ; For the incoming pointer
          ((pair? thing)
           (seen! thing)
           (+ 1 (loop (car thing))
              (loop (cdr thing))))
          ((vector? thing)
           (seen! thing)
           (+ 1 (sum (map loop (vector->list thing)))))
          ((record? thing)
           (seen! thing)
           ;; Records appear to have an overhead of 2
           (+ 1 2 (sum (map loop (record-fields thing)))))
          (else 1))))

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

;; filter-tree :: (a -> Bool) x (cons-tree a) -> [a]
;; filter-tree does not preserve the tree structure of the input.
(define (filter-tree pred tree)
  (let walk ((tree tree) (answer '()))
    (cond ((pair? tree)
           (walk (car tree) (walk (cdr tree) answer)))
          ((null? tree)
           answer)
          ((pred tree)
           (cons tree answer))
          (else answer))))

;; filter-map-tree :: (a -> b) x (cons-tree a) -> [b]
;; filter-map-tree does not preserve the tree structure of the input.
(define (filter-map-tree proc tree)
  (let walk ((tree tree) (answer '()))
    (cond ((pair? tree)
           (walk (car tree) (walk (cdr tree) answer)))
          ((null? tree)
           answer)
          (else
           (let ((elt (proc tree)))
             (if elt
                 (cons elt answer)
                 answer))))))

(define (assert pred)
  (if (not pred)
      (error "Assertion failed")))

(define (force-assq key lst)
  (let ((binding (assq key lst)))
    (if binding
        (cdr binding)
        (error "Did not find" key lst))))

(define (fmap-maybe f object)
  (if object (f object) #f))

(define-syntax begin1
  (syntax-rules ()
    ((_ form1 form ...)
     (let ((answer form1))
       form ...
       answer))))

(define-syntax abegin1
  (sc-macro-transformer
   (lambda (exp env)
     (let ((object (close-syntax (cadr exp) env))
	   (forms (map (lambda (form)
                         (make-syntactic-closure env '(it) form))
                       (cddr exp))))
       `(let ((it ,object))
	  ,@forms
          it)))))

(define-syntax when
  (syntax-rules ()
    ((_ test form ...)
     (if test
         (let () form ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ test form ...)
     (if (not test)
         (let () form ...)))))
