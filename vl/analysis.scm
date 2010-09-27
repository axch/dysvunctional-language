(define-structure (analysis (safe-accessors #t))
  bindings)

(define (analysis-search exp env analysis win lose)
  (let loop ((bindings (analysis-bindings analysis)))
    (if (null? bindings)
	(lose)
	(if (and (equal? exp (caar bindings))
		 (abstract-equal? env (cadar bindings)))
	    (win (caddar bindings))
	    (loop (cdr bindings))))))

;;; ANALYSIS-GET is \bar E_1 from the paper.
(define (analysis-get exp env analysis)
  (analysis-search exp env analysis
   (lambda (value)
     value)
   (lambda ()
     abstract-all)))

;;; EXPAND-ANALYSIS is \bar E_1' from the paper.
;;; It registers interest in the evaluation of EXP in ENV by producing
;;; a binding to be added to the new incarnation of ANALYSIS, should
;;; the current incarnation lack any binding already covering that
;;; question.
(define (analysis-expand exp env analysis)
  (analysis-search exp env analysis
   (lambda (value)
     '())
   (lambda ()
     (list (list exp env abstract-all)))))

(define (same-analysis-binding? binding1 binding2)
  (abstract-equal? binding1 binding2))

(define (same-analysis? ana1 ana2)
  (lset= same-analysis-binding? (analysis-bindings ana1)
	 (analysis-bindings ana2)))
