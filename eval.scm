;;; Driver loop

(define ad-user-environment #f)

(define (start-ad)
  (set! *epsilon-count* 0)
  (run-ad))

(define (run-ad)
  (if (not ad-user-environment)
      (set! ad-user-environment (make-ad-user-environment)))
  (display ";;; Ad> ")
  (let ((answer (perturbed-eval (read) ad-user-environment the-non-perturbation)))
    (display ";;; Ad value: ")
    (write answer)
    (newline))
  (run-ad))

(define (perturbed-eval form env epsilon)
  (cond ((symbol? form)
	 (perturbed-lookup form env epsilon))
	((pair? form)
	 (cond ((eq? (car form) 'quote)
		(scheme-value->perturbed-eval-value (cadr form) epsilon))
	       ((eq? (car form) 'lambda)
		(eval-ad-lambda form env)) ;; Really?
	       ((eq? (car form) 'define)
		(eval-perturbed-definition form env))
	       ((eq? (car form) 'if)
		(eval-perturbed-if form env epsilon))
	       (else
		(eval-perturbed-application form env epsilon))))
	(else
	 (scheme-value->perturbed-eval-value form epsilon))))

(define (eval-ad-lambda form env)
  (let ((formals (cadr form))
	(body (caddr form))) ; TODO Only one-form bodies in ad-lambda
    (make-ad-compound body env formals)))

(define (eval-perturbed-application form env epsilon)
  (let ((procedure (perturbed-eval (car form) env epsilon))
	(arguments (map (lambda (subform)
			  (perturbed-eval subform env epsilon))
			(cdr form))))
    (perturbed-apply procedure arguments epsilon)))

(define (perturbed-apply procedure arguments epsilon)
  (let ((make-dual (gen-make-dual epsilon))
	(primal (gen-primal epsilon))
	(perturbation (gen-perturbation epsilon)))
    (cond ((j*? (primal* procedure))
	   (apply-j* (car arguments) epsilon))
	  ((ad-primitive? (primal* procedure))
	   (if (or (not (numeric? procedure)) (non-perturbation? epsilon))
	       (apply (ad-primitive-implementation procedure) arguments)
	       (make-dual
		(perturbed-apply
		 (primal procedure)
		 (map primal arguments)
		 (epsilon-parent epsilon))
		(do-multiply
		 (perturbed-apply (jacobian (primal* procedure))
				  (map primal arguments)
				  (epsilon-parent epsilon))
		 (map perturbation arguments)))))
	  ((ad-compound? (primal* procedure))
	   (let ((procedure (primal* procedure)))
	     (perturbed-eval
	      (ad-compound-body procedure)
	      (ad-extend-environment
	       (ad-compound-env procedure)
	       (ad-compound-formals procedure)
	       arguments)
	      epsilon)))
	  ((perturbing-compound? procedure)
	   ;; Do I want the perturbation used to evaluate a procedure
	   ;; that comes out of j* to be determined by the procedure
	   ;; when it's created, or by the interpreter when it goes to
	   ;; run it?  Right now it doesn't matter because all uses of
	   ;; j* apply the resulting procedure immediately and only
	   ;; once.
	   (let ((epsilon* (perturbing-compound-epsilon procedure)))
	     (let ((make-dual (gen-make-dual epsilon*))
		   (primal (gen-primal epsilon*))
		   (perturbation (gen-perturbation epsilon*)))
	       (let ((answer
		      (perturbed-apply (perturbing-compound-base procedure)
				       (list (make-dual (car arguments) (cadr arguments)))
				       epsilon*)))
		 (cons (primal answer) (perturbation answer))))))
	  (else
	   (error "Not an ad-procedure" procedure)))))

(define (do-multiply J perturbation)
  (if (not (list? J))
      (do-multiply (list J) perturbation)
      (if (not (= (length J) (length perturbation)))
	  (error "Size mismatch" J perturbation)
	  (apply plus-perturbation (map times-perturbation J perturbation)))))

(define (times-perturbation number perturbation)
  (cond ((and (number? number) (number? perturbation))
	 (* number perturbation))
	((or (perturbed? number) (perturbed? perturbation))
	 (let ((epsilon (max (perturbed-epsilon number) (perturbed-epsilon perturbation))))
	   (let ((primal (gen-primal epsilon))
		 (pert (gen-perturbation epsilon)))
	     ((gen-make-dual epsilon)
	      (times-perturbation (primal number) (primal perturbation))
	      (plus-perturbation
	       (times-perturbation (primal number) (pert perturbation))
	       (times-perturbation (pert number) (primal perturbation)))))))
	(else (error "Ack!  times-perturbation can't handle" number perturbation))))

(define (plus-perturbation . args)
  (cond ((every number? args)
	 (apply + args))
	((any perturbed? args)
	 (let ((epsilon (apply max (map perturbed-epsilon args))))
	   ((gen-make-dual epsilon)
	    (apply plus-perturbation (map (gen-primal epsilon) args))
	    (apply plus-perturbation (map (gen-perturbation epsilon) args)))))
	(else (error "Ack!  plus-perturbation can't handle" args))))

(define (eval-perturbed-if form env epsilon)
  (let ((predicate (cadr form))
	(consequent (caddr form))
	(alternate (cadddr form)))
    (if (primal* (perturbed-eval predicate env epsilon))
	(perturbed-eval consequent env epsilon)
	(perturbed-eval alternate env epsilon))))

(define (eval-perturbed-definition form env)
  (error "TODO definitions not supported in perturbed-eval yet"))

(define (apply-j* procedure parent-epsilon)
  (make-perturbing-compound
   procedure
   (make-fresh-epsilon parent-epsilon)))

(define-structure perturbing-compound
  base
  epsilon)
