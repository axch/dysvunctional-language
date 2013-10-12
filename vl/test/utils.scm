;;; ----------------------------------------------------------------------
;;; Copyright 2010-2011 National University of Ireland.
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
;;; Invariants:
;;;   macroexpand is idempotent
;;;   macroexpand produces syntactically correct kernel language
;;;   interpret produces the same answer as analyze-generate-eval
;;;   for solved programs, analyze binds the answer to the program
;;;     and generate emits `(begin ,answer).
;;;   each stage of FOL compilation does not affect the answer
;;;   each stage of FOL compilation is idempotent
;;;   any chain of FOL stages ending in TIDY is idempotent
;;;   for union-free programs, optimized FOL does not cons
;;;   the code generator emits syntactically correct and type correct FOL
;;;   FOL compilation preserves syntactic and type correctness stepwise

(load-relative "../../testing/load" fol-environment)
(load-relative-compiled "../../fol/test/utils" fol-environment)
;; TODO This snippet is duplicated with fol/test/load.scm
(let ((client-environment (the-environment)))
  (for-each
   (lambda (export)
     (environment-define
      client-environment export (environment-lookup fol-environment export)))
   '(;; Testing adverbs
     carefully
     meticulously
     )))

(define (careful-macroexpand program)
  (abegin1
   (macroexpand program)
   (check (equal? it (macroexpand it)))))

(define (meticulous-solved-analyze kernel-program answer)
  (let ((analysis (analyze kernel-program)))
    (let loop ((bindings (analysis-bindings analysis)))
      (cond ((null? bindings)
             (error "Analysis makes no binding for the original program"
                    program))
            ((and (eval-binding? (car bindings))
                  (equal? kernel-program (binding-exp (car bindings))))
             (check (equal? answer (binding-value (car bindings))))
             analysis)
            (else (loop (cdr bindings)))))))

(define (meticulous-generate program analysis answer)
  (abegin1
   (generate program analysis)
   (check (equal? answer (fol-eval it)))))

(define (meticulous-solved-generate program analysis answer)
  (abegin1
   (strip-argument-types
    (meticulous-generate program analysis answer))
   (check (equal? `(begin ,answer) it))))

(define (determined-answer program)
  (let* ((kernel (careful-macroexpand program))
         (answer (interpret kernel))
         (analysis (meticulous-solved-analyze kernel answer))
         (raw-fol (meticulous-solved-generate kernel analysis answer)))
    answer))

(define (compile-carefully program)
  (let* ((kernel (careful-macroexpand program))
         (analysis (analyze kernel))
         (raw-fol (generate kernel analysis)))
    (fol-optimize raw-fol carefully)))

(define (compile-meticulously program)
  (let* ((kernel (careful-macroexpand program))
         (answer (interpret kernel))
         (analysis (analyze kernel))
         (raw-fol (meticulous-generate kernel analysis answer)))
    (fol-optimize raw-fol (meticulously answer))))

(define ((union-free-answerer compile) program
         #!optional wallpaper? no-gensyms?)
  (if (default-object? wallpaper?)
      (set! wallpaper? #f))
  (if (default-object? no-gensyms?)
      (set! no-gensyms? #f))
  (if wallpaper?
      (begin
        (display "***NEW PROGRAM ***")
        (newline)
        (pp program)))
  (let ((opt-fol (compile program)))
    (if wallpaper? (pp opt-fol))
    ;; In the union-free case, SRA is successful at removing all
    ;; internal consing.
    (check (not (occurs-in-tree? 'car opt-fol)))
    (check (not (occurs-in-tree? 'cdr opt-fol)))
    (check (not (occurs-in-tree? 'vector-ref opt-fol)))

    (if no-gensyms?
        (check (not (occurs-in-tree? 'gensym opt-fol))))

    (fol-eval opt-fol)))

(define union-free-answer (union-free-answerer compile-meticulously))
(define loose-union-free-answer (union-free-answerer compile-carefully))

(define (analyzed-answer program)
  (let ((full-prog (macroexpand program)))
    (let loop ((bindings (analysis-bindings (analyze program))))
      (cond ((null? bindings)
             (error "Analysis makes no binding for the original program"
                    program))
            ((and (eval-binding? (car bindings))
                  (equal? full-prog (binding-exp (car bindings))))
             (binding-value (car bindings)))
            (else (loop (cdr bindings)))))))

(define (for-each-example filename proc)
  (with-input-from-file filename
    (lambda ()
      (let* ((first (read))
             (second (read)))
        (let loop ((first first)
                   (second second)
                   (third (read))
                   (definitions '()))
          (cond ((eof-object? first)
                 'done)
                ((definition? first)
                 (loop second third (read) (cons first definitions)))
                ((include-directive? first)
                 (loop
                  second
                  third
                  (read)
                  (append (reverse (with-working-directory-pathname
                                    (directory-namestring filename)
                                    (lambda ()
                                      (read-source (cadr first)))))
                          definitions)))
                ((eq? '===> second)
                 (proc `(let ()
                          ,@(reverse definitions)
                          ,first)
                       third)
                 (let* ((new-first (read))
                        (new-second (read)))
                   (loop new-first new-second (read) definitions)))
                (else
                 (proc `(let ()
                          ,@(reverse definitions)
                          ,first))
                 (loop second third (read) definitions))))))))

(define *compilation-results-wallp* #f)

(define (define-union-free-example-test program #!optional value)
  (if (not (default-object? value))
      (define-test
        (check (equal? value (union-free-answer program *compilation-results-wallp* #t))))
      (define-test
        ;; At least check that interpret and compile-to-fol agree
        (union-free-answer program *compilation-results-wallp* #t))))

(define (define-loose-union-free-example-test program #!optional value)
  (if (not (default-object? value))
      (define-test
        (check (equal? value (loose-union-free-answer program *compilation-results-wallp* #t))))
      (define-test
        ;; At least check the FOL invariants are obeyed
        (loose-union-free-answer program *compilation-results-wallp* #t))))
