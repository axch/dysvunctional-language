;;; -*- Mode: Scheme -*-

;;;; Rudimentary Statistical Profiler for MIT Scheme

;;; Copyright 2009, Taylor R. Campbell.
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; This rudimentary statistical profiler periodically interrupts the
;;; program and records two numbers for each interrupted compiled[*]
;;; entry found on the stack:
;;;
;;; 1. The `sampled' count, which is the number of times that the
;;;    compiled entry was the interrupted one.  This tells you how
;;;    often a particular part of the code is hit, which approximately
;;;    tells you how important it is for that code to be fast.
;;;
;;; 2. The `waiting' count, which is the number of times that the
;;;    compiled entry was found on the stack as the return address of
;;;    a continuation.  This tells you how much often the profiler hit
;;;    something involved in computing a particular expression, which
;;;    approximately tells you how much time is spent computing that
;;;    expression.
;;;
;;; To profile the evaluation an expression <expression>, sampling at
;;; every <sample-interval> millisceonds, and then to display its
;;; profile and yield its value, evaluate
;;;
;;;   (WITH-PROFILING <sample-interval> (LAMBDA () <expression>)).
;;;
;;; A slightly more sophisticated profiler might record a directed
;;; graph of edges from `callers' to `callees' labelled by the number
;;; of times the edge was found on the stack (really, not edges from
;;; callers to callees, but edges from continuations, labelled by the
;;; number of times that one continuation was found as that of a
;;; subsubproblem of the subproblem of the other continuation).  This
;;; is not such a sophisticated profiler.
;;;
;;; This profiler uses the full-blown stack parser, which is a fairly
;;; heavy-weight abstraction not really fit for use in high-frequency
;;; sampling when really only the return addresses on the stack and
;;; their debugging information are important, not any dynamic state.
;;; Probably as a consequence of this, programs run significantly
;;; slower while being profiled.
;;;
;;; [*] Yes, this works only in compiled code.  It is not clear how to
;;; identify points in interpreted code when recording samples.  But
;;; if your code runs too slowly interpreted, the first step should be
;;; to compile it, not to profile it, because that will always make it
;;; run faster without requiring you to change your code.

(declare (usual-integrations))

;;;; Miscellaneous Kludgerosity

(define special-form-procedure-name?
  (environment-lookup (->environment '(RUNTIME COMPILER-INFO))
                      'SPECIAL-FORM-PROCEDURE-NAME?))

(define (compiled-entry? object)
  (let-syntax ((ucode-type
                (sc-macro-transformer
                 (lambda (form environment)
                   environment          ;ignore
                   (apply microcode-type (cdr form))))))
    (object-type? (ucode-type compiled-entry) object)))

(define event-return-address 'UNINITIALIZED)

(let ((blocked? (block-thread-events)))
  (signal-thread-event (current-thread)
    (lambda ()
      (call-with-current-continuation
        (lambda (continuation)
          (set! event-return-address
                (let ((stack-frame
                       ;; Total kludge here.  If thread.scm changes,
                       ;; this will have to change too.  Note that
                       ;; this magic subproblem skippage is not
                       ;; isolated to here -- it must be done in
                       ;; FIND-FIRST-SUBPROBLEM, too, because here we
                       ;; use SUSPEND-CURRENT-THREAD to force the
                       ;; event to run, while during sampling the
                       ;; event is run by a timer interrupt, which has
                       ;; a somewhat different-looking continuation.
                       (stack-frame/next-subproblem
                        (continuation/first-subproblem continuation))))
                  (and (eq? stack-frame-type/compiled-return-address
                            (stack-frame/type stack-frame))
                       (stack-frame/return-address stack-frame))))))))
  (do () ((not (eq? event-return-address 'UNINITIALIZED)))
    (suspend-current-thread))
  (if (not blocked?)
      (unblock-thread-events)))

(define profiler:debug-internal-errors? #f)
(define profiler:show-expressions? #t)

;;;; Running with Profiling

(define (run-profiling sample-interval thunk)
  (let ((profile (make-profile))
        (timer-registration #t))
    (reset-sample-times!)
    (define (register-event)
      (if timer-registration
          (set! timer-registration
                (register-timer-event sample-interval
                  (lambda ()
		    (sample-time!)
                    (call-with-current-continuation
                      (lambda (continuation)
                        (carefully-record-sample profile continuation)
                        (register-event))))))))
    (define (deregister-event)
      (deregister-timer-event timer-registration)
      (set! timer-registration #f)
      (sample-time!))
    (values (with-simple-restart 'ABORT "Abort profiling."
              (lambda ()
                (dynamic-wind register-event
                              (lambda () (with-profiling-continuation thunk))
                              deregister-event)))
            profile)))

(define (carefully-record-sample profile continuation)
  (with-simple-restart 'CONTINUE "Ignore the sample."
    (lambda ()
      (define (go) (record-sample profile continuation))
      (if profiler:debug-internal-errors?
          (go)
          (bind-condition-handler (list condition-type:error)
              (lambda (condition)
                (write-notification-line
                 (lambda (output-port)
                   (write-string "Error in profiler: " output-port)
                   (write-condition-report condition output-port)))
                (continue))
            go)))))

(define (profiler-interrupt-stack-frame? stack-frame)
  (let ((return-address event-return-address))
    (and (compiled-return-address? return-address)
         (eq? stack-frame-type/compiled-return-address
              (stack-frame/type stack-frame))
         (eq? event-return-address (stack-frame/return-address stack-frame)))))

(define profiling-return-address #f)

(define (profiling-stack-frame? stack-frame)
  (let ((return-address profiling-return-address))
    (and (compiled-return-address? return-address)
         (eq? stack-frame-type/compiled-return-address
              (stack-frame/type stack-frame))
         (eq? return-address (stack-frame/return-address stack-frame)))))

(define (with-profiling-continuation thunk)
  ;; Calling IDENTITY-PROCEDURE here creates a continuation with a
  ;; return address unique to this code, which we use to determine
  ;; where to stop walking down the stack while profiling.
  (identity-procedure
   (call-with-current-continuation
     (lambda (continuation)
       (let ((stack-frame (continuation/first-subproblem continuation)))
         (if (eq? stack-frame-type/compiled-return-address
                  (stack-frame/type stack-frame))
             (fluid-let ((profiling-return-address
                          (stack-frame/return-address stack-frame)))
               (thunk))
             (thunk)))))))

;;;; Profile Data

(define-structure (profile
                   (conc-name profile.)
                   (constructor make-profile ()))
  (sampled (make-strong-eq-hash-table) read-only #t)
  (waiting (make-strong-eq-hash-table) read-only #t))

(define-structure (datum
                   (conc-name datum.)
                   (constructor make-datum
                                (return-address
                                 expression
                                 subexpression
                                 environment-names)))
  (count 0)
  (return-address #f read-only #t)
  (expression #f read-only #t)
  (subexpression #f read-only #t)
  (environment-names #f read-only #t))

(define (record-sample profile continuation)
  (let ((stack-frame
         (find-first-subproblem (continuation->stack-frame continuation))))
    (if stack-frame
        (begin
          (record-datum (profile.sampled profile) stack-frame)
          (let loop ((stack-frame stack-frame))
            (let ((stack-frame (find-next-subproblem stack-frame)))
              (if (and stack-frame (not (profiling-stack-frame? stack-frame)))
                  (begin (record-datum (profile.waiting profile) stack-frame)
                         (loop stack-frame)))))))))

(define (find-first-subproblem stack-frame)
  (let loop ((next (stack-frame/skip-non-subproblems stack-frame)))
    (cond ((profiler-interrupt-stack-frame? next)
           ;; Another kludge about the internals of thread.scm.
           (cond ((stack-frame/next-subproblem next) => find-next-subproblem)
                 (else #f)))
          ((stack-frame/next-subproblem next) => loop)
          (else (find-subproblem stack-frame)))))

(define (find-subproblem stack-frame)
  (if (compiled-entry? (stack-frame/return-address stack-frame))
      stack-frame
      (find-next-subproblem stack-frame)))

(define (find-next-subproblem stack-frame)
  (cond ((stack-frame/next-subproblem stack-frame) => find-subproblem)
        (else #f)))

(define (record-datum hash-table stack-frame)
  ((lambda (datum) (set-datum.count! datum (+ 1 (datum.count datum))))
   (let ((return-address (stack-frame/return-address stack-frame)))
     (if (compiled-entry? return-address)
         (let ((return-address
                (if (compiled-closure? return-address)
                    (compiled-closure->entry return-address)
                    return-address)))
           (hash-table/intern! hash-table return-address
             (lambda ()
               (receive (expression environment subexpression)
                   (stack-frame/debugging-info stack-frame)
                 (make-datum return-address
                             expression
                             subexpression
                             (environment-ancestry-names environment))))))
         ;; What to do for interpreted code?  Fetch the debugging
         ;; information and use the expression, subexpression, and
         ;; environment ancestry names as the key?
         (make-datum #f #f #f #f)))))

;;;; Display

(define (with-profiling sample-interval thunk)
  (receive (value profile)
      (with-notification (lambda (output-port)
                           (write-string "Profiling" output-port))
        (lambda ()
          (run-profiling sample-interval thunk)))
    (write-notification-line
     (lambda (output-port)
       (display-profile profile output-port)))
    value))

(define (display-profile profile output-port)
  (define (sortem data)
    (sort data (lambda (a b) (< (datum.count a) (datum.count b)))))
  (let ((sampled (sortem (hash-table/datum-list (profile.sampled profile))))
        (waiting (sortem (hash-table/datum-list (profile.waiting profile)))))
    (let ((total-sampled (reduce + 0 (map datum.count sampled)))
          (total-waiting (reduce + 0 (map datum.count waiting))))
      (write total-sampled output-port)
      (display " samples" output-port)
      (newline output-port)
      (display-profile-data "Waiting" waiting total-waiting output-port)
      (display-profile-data "Sampled" sampled total-sampled output-port))))

(define (display-profile-data title data total output-port)
  total                                 ;ignore
  (newline output-port)
  (display "*** " output-port)
  (display title output-port)
  (newline output-port)
  (newline output-port)
  (for-each (lambda (count-string datum)
              (write-string count-string output-port)
              (write-string " sample" output-port)
              (if (not (= 1 (datum.count datum)))
                  (write-char #\s output-port))
              (write-string " in " output-port)
              (let ((environment-names (datum.environment-names datum)))
                (if (pair? environment-names)
                    (show-environment-names environment-names output-port)
                    (write (datum.return-address datum) output-port)))
              (if profiler:show-expressions?
                  (begin
                    (write-char #\: output-port)
                    (newline output-port)
                    (show-profile-expression (datum.expression datum)
                                             (datum.subexpression datum)
                                             output-port)))
              (newline output-port))
            (data-count-strings data)
            data))

(define (data-count-strings data)
  (let ((count-strings
         (map (lambda (datum) (number->string (datum.count datum))) data)))
    (map (let ((width (reduce max 0 (map string-length count-strings))))
           (lambda (count-string)
             (string-pad-left count-string width #\space)))
         count-strings)))

(define (environment-ancestry-names environment)
  (let recur ((environment environment))
    (if (environment? environment)      ;Idle paranoia?
        (let ((package (environment->package environment)))
          (if package
              (list (package/name package))
              (let ((name (environment-procedure-name environment))
                    (names
                     (if (environment-has-parent? environment)
                         (recur (environment-parent environment))
                         '())))
                (if name
                    (cons (cond ((special-form-procedure-name? name)
                                 => (lambda (rename) (list (intern rename))))
                                (else name))
                          names)
                    names))))
        '())))

(define (show-environment-names environment-names output-port)
  (if (pair? environment-names)
      (write-string
       (decorated-string-append "" ", " ""
         (map write-to-string (reverse environment-names)))
       output-port)))

(define (show-profile-expression expression subexpression output-port)
  (write-string " evaluating " output-port)
  (let ((description (invalid-expression-description expression)))
    (cond (description
           (write-string description output-port)
           (newline output-port))
          ((or (debugging-info/undefined-expression? subexpression)
               (debugging-info/unknown-expression? subexpression))
           (newline output-port)
           (profiler-pp expression output-port))
          (else
           (newline output-port)
           (profiler-pp subexpression output-port)
           (write-string " for ### in " output-port)
           (newline output-port)
           (profiler-pp
            (unsyntax-with-substitutions expression
                                         (list (cons subexpression '|###|)))
            output-port)))))

(define (invalid-expression-description expression)
  (cond ((debugging-info/compiled-code? expression)
         ;++ Should this display the compiled entry itself?
         "compiled code")
        ((debugging-info/undefined-expression? expression)
         "undefined expression")
        (else #f)))

(define (profiler-pp expression output-port)
  ;; Random parametrization.
  (fluid-let ((*unparser-list-breadth-limit* 5)
              (*unparser-list-depth-limit* 3)
              (*unparser-string-length-limit* 40)
              (*unparse-primitives-by-name?* #t)
              (*pp-save-vertical-space?* #t)
              (*pp-default-as-code?* #t))
    (pp expression output-port)))

(define *sample-times* '())

(define (sample-time-differences)
  (let loop ((samples *sample-times*)
	     (answers '()))
    (if (null? (cdr samples))
	answers
	(loop (cdr samples)
	      (cons (- (car samples) (cadr samples))
		    answers)))))

(define (reset-sample-times!)
  (set! *sample-times* '())
  (sample-time!))

(define (sample-time!)
  #;(set! *sample-times* (cons (real-time-clock) *sample-times*))
  'ok)

;;; TODO Taylor says:

;; What is the garbage collector up to during the profiling?  Can you
;; record a list of the values of (car (gc-timestamp)), like
;; *SAMPLE-TIMES*?

;; If you want to pursue this hypothesis [that deep recursions are
;; causing the problem] further, you could instrument the profiler to
;; record a histogram of stack depths; it would be reasonably
;; straightforward.

