(declare (usual-integrations))
;;;; Reader

;;; This is a little wrapper around the underlying MIT Scheme reader
;;; that does two interesting things: it parses (include "foo")
;;; directives (at the top level only) by recursively reading those
;;; files and splicing their contents into the current one (beware, no
;;; loop detection); and it interprets the symbol ===> as an
;;; s-expression comment when used at the top level.  The latter is
;;; for compatibility with a testing framework that would use that
;;; symbol to delimit expected answers.

(define (default-extension-to-vlad pathname)
  (merge-pathnames (->pathname pathname) (->pathname "foo.vlad")))

(define (include-directive? form)
  (and (list? form)
       (= (length form) 2)
       (eq? (first form) 'include)
       (string? (second form))))

(define (include-definitions-directive? form)
  (and (list? form)
       (= (length form) 2)
       (eq? (first form) 'include-definitions)
       (string? (second form))))

(define (read-source pathname)
  (let ((pathname (default-extension-to-vlad pathname)))
    (with-input-from-file pathname
      (lambda ()
        (let loop ((forms '()))
          (let ((form (read)))
            (if (eof-object? form)
                (expand-toplevel-source (reverse forms) pathname)
                (loop (cons form forms)))))))))

(define (expand-toplevel-source forms pathname)
  (let loop ((forms forms) (ignore? #f))
    (if (null? forms)
        '()
        (let ((form (car forms)) (rest (cdr forms)))
          (cond
           (ignore? (loop rest #f))
           ((eq? '===> form) (loop rest #t))
           ((include-directive? form)
            (append (with-working-directory-pathname
                     (directory-namestring pathname)
                     (lambda ()
                       (read-source (second form))))
                    (loop rest #f)))
           ((include-definitions-directive? form)
            (append (filter definition?
                     (with-working-directory-pathname
                      (directory-namestring pathname)
                      (lambda ()
                        (read-source (second form)))))
                    (loop rest #f)))
           (else (cons form (loop rest #f))))))))
