(define (inquire-eval exp env analysis)
  (cond ((constant? exp) '())
        ((variable? exp) '())
        ((lambda? exp) '())
        ((pair-form? exp)
         (lset-union same-analysis-binding?
          (ana-inquire
           (car-subexp exp) env analysis)
          (ana-inquire
           (cdr-subexp exp) env analysis)))
        ((application? exp)
         (let ((ator (operator-subexp exp))
               (rand (operand-subexp exp)))
           (lset-union same-analysis-binding?
            (ana-inquire ator env analysis)
            (ana-inquire rand env analysis)
            (ana-inquire
             (ana-investigate ator env analysis)
             (ana-investigate rand env analysis)
             analysis))))))

(define (inquire-apply proc arg analysis)
  (cond ((none? arg) '())
        ((none? proc) '())
        ((primitive? proc)
         ((primitive-inquire-impl proc)
          arg analysis))
        ((closure? proc)
         (ana-inquire
          (closure-body proc)
          (extend-env (closure-formal proc)
                      arg (closure-env proc))
          analysis))))
(define (investigate-eval exp env analysis)
  (cond ((constant? exp) (constant-value exp))
        ((variable? exp) (lookup exp env))
        ((lambda? exp) (make-closure exp env))
        ((pair-form? exp)
         (let ((car-answer
                (ana-investigate
                 (car-subexp exp) env analysis))
               (cdr-answer
                (ana-investigate
                 (cdr-subexp exp) env analysis)))
           (if (and (not (none? car-answer))
                    (not (none? cdr-answer)))
               (cons car-answer cdr-answer)
               none)))  ; strict
        ((application? exp)
         (ana-investigate
          (ana-investigate
           (operator-subexp exp) env analysis)
          (ana-investigate
           (operand-subexp exp) env analysis)
          analysis))))
(define (investigate-apply proc arg analysis)
  (cond ((none? arg) none)  ; strict
        ((none? proc) none)
        ((primitive? proc)
         ((primitive-abstract-impl proc)
          arg analysis))
        ((closure? proc)
         (ana-investigate
          (closure-body proc)
          (extend-env (closure-formal proc)
                      arg (closure-env proc))
          analysis))))

(define (ana-investigate key1 key2 analysis)
  (ana-search key1 key2 analysis
   binding-value (lambda () none)))

(define (ana-inquire key1 key2 analysis)
  (ana-search key1 key2 analysis
   (lambda (binding)
     '())
   (lambda ()
     (if (or (none? key1) (none? key2))
         '()
         (list (make-binding key1 key2 none))))))

(define ((investigate-binding analysis) binding)
  (let ((part1 (binding-part1 binding))
        (part2 (binding-part2 binding))
        (value (binding-value binding)))
    (let ((new-value ((if (eval-binding? binding)
                          investigate-eval
                          investigate-apply)
                      part1 part2 analysis)))
      (make-binding part1 part2
       (abstract-union value new-value)))))

(define (investigate-analysis analysis)
  (map (investigate-binding analysis)
       (ana-bindings analysis)))

(define ((inquire-binding analysis) binding)
  (let ((part1 (binding-part1 binding))
        (part2 (binding-part2 binding)))
    ((if (eval-binding? binding)
         inquire-eval
         inquire-apply)
     part1 part2 analysis)))

(define (inquire-analysis analysis)
  (apply lset-union same-analysis-binding?
         (map (inquire-binding analysis)
              (ana-bindings analysis))))

(define (step analysis)
  (make-analysis
   (lset-union same-analysis-binding?
               (investigate-analysis analysis)
               (inquire-analysis analysis))))

(define (analyze program)
  (let ((initial-analysis
         (make-analysis
          (list (make-binding
                 (macroexpand program)
                 (initial-user-env)
                 none)))))
    (let loop ((old-analysis initial-analysis)
               (new-analysis
                (step initial-analysis)))
      (if (step-changed-analysis?
           old-analysis new-analysis)
          (loop new-analysis
                (step new-analysis))
          new-analysis))))
