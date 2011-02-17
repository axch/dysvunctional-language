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

(define (read-source pathname)
  (let ((pathname (default-extension-to-vlad pathname)))
    (call-with-input-file pathname
      (lambda (input-port)
	(let loop ((forms '()) (ignore? #f))
	  (let ((form (read input-port)))
	    (cond
	     ((eof-object? form) (reverse forms))
	     (ignore? (loop forms #f))
	     ((eq? '===> form) (loop forms #t))
	     ((include-directive? form)
	      (loop
	       (append (reverse (with-working-directory-pathname
				 (directory-namestring pathname)
				 (lambda ()
				   (read-source (second form)))))
		       forms)
	       #f))
	     (else (loop (cons form forms) #f)))))))))
