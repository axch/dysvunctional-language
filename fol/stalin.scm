;;; ----------------------------------------------------------------------
;;; Copyright 2010-2011 National University of Ireland; 2013 Alexey Radul.
;;; ----------------------------------------------------------------------
;;; This file is part of DysVunctional Language.
;;; 
;;; DysVunctional Language is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;;  License, or (at your option) any later version.
;;; 
;;; DysVunctional Language is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Affero General Public License
;;; along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

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
    (force-shell-command (string-append "stalin -On -d -q -copt -O2 " (->namestring output-file)))))

(define (run-stalin #!optional output-base)
  (if (default-object? output-base)
      (set! output-base "stanozzle"))
  (force-shell-command (->namestring (pathname-new-type output-base #f))))

(define replace-let-values
  (rule-simplifier
   (list
    (rule `(let-values (((? names) (? exp)))
             (?? body))
          `(call-with-values ,exp
             (lambda ,names
               ,@body))))))

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
   (stalinize
    (replace-nulls
     (replace-let-values
      (strip-begin
       (strip-argument-types
        (structure-definitions->vectors ; Does Stalin even support user product types?
         program))))))))

;;; TODO This is very similar to flonumize from mit-scheme.scm; abstract commonalities
(define (stalinize program)
  (define read-real-call? (tagged-list? 'read-real))
  (define (loop expr)
    (cond ((number? expr) (exact->inexact expr))
          ((accessor? expr) `(,(car expr) ,(loop (cadr expr)) ,@(cddr expr)))
          ((read-real-call? expr)
           `(exact->inexact ,expr))
          ((pair? expr) (map loop expr))
          (else expr)))
  (loop program))

;;; On the subject of getting the results to perform, David says:

;;; Improving Stalin's output is an art unto itself.  Step 1 would be
;;; to look for anything in the FOL code that can be interpreted with
;;; any kind of type ambiguity; in particular, I'd bet money on
;;; exact/inexact issues.

;;; The way you print constants could be introducing artifacts that
;;; are surprising.

;;; If you had a pair that was sometimes exact and sometimes inexact
;;; in a list it would lead to a polymorphic type.  There may also be
;;; compilation flags which affect the analyses needed.  This is black
;;; magic and one of the things I hate about Stalin.  You should load
;;; up stalin-mode.el as well, and get the compiler to dump its
;;; optimization DB about the FOL.  You can browses all the inferred
;;; types then and see where they arise.  It's on the same order of
;;; difficulty as reading a hex dump of a kernel panic.
