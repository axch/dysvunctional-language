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
