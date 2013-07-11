(declare (usual-integrations))
(declare (integrate-external "syntax"))
(declare (integrate-external "../support/pattern-case/pattern-case"))
;;;; Dead type elimination

;;; Eliminating unused type declarations

(define (%dead-type-elimination program)
  (if (begin-form? program)
      (let ((live-types (make-eq-hash-table))
            (queue '())
            (type-map (type-map program)))
        (define (live-type! type)
          (cond ((null? type) 'ok)
                ((symbol? type)
                 (hash-table/lookup live-types type
                  (lambda (datum)
                    'ok)
                  (lambda ()
                    (hash-table/put! live-types type #t)
                    (set! queue (cons type queue)))))
                (else
                 (for-each live-type! (type-factors type)))))
        (define (a-live-type!)
          (if (null? queue)
              #f
              (begin1 (car queue)
                (set! queue (cdr queue)))))
        (for-each-fol-expression program
         (lambda (expression type)
           (live-type! type)))
        (for-each
         (rule `(define (? stuff)
                  (argument-types (?? types))
                  (? body))
               (for-each live-type! types))
         program)
        (let loop ((next (a-live-type!)))
          (if next
              (begin
                (live-type! (hash-table/get type-map next #f))
                (loop (a-live-type!)))))
        (tidy-begin
         (filter (lambda (item)
                   (or (not (type-definition? item))
                       (hash-table/get live-types (cadr item) #f)))
                 program)))
      program))
