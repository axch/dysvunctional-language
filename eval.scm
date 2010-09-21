(define (ad-eval form env)
  (cond ((symbol? form)
	 (ad-lookup form env))
	((pair? form)
	 (cond ((eq? (car form) 'quote)
		(scheme-value->ad-eval-value (cadr form)))
	       ((eq? (car form) 'lambda)
		(eval-ad-lambda form env))
	       ((eq? (car form) 'define)
		(eval-ad-definition form env))
	       ((eq? (car form) 'if)
		(eval-ad-if form env))
	       (else
		(eval-ad-application form env))))
	(else
	 (scheme-value->ad-eval-value form))))

(define (eval-ad-application form env)
  (let ((procedure (ad-eval (car form) env))
	(arguments (map (lambda (subform)
			  (ad-eval subform env))
			(cdr form))))
    (ad-apply procedure arguments)))

(define (ad-apply procedure arguments)
  (cond ((ad-primitive? procedure)
	 (apply (ad-primitive-implementation procedure) arguments))
	((ad-compound? procedure)
	 (ad-eval (ad-compound-body procedure)
		  (ad-extend-environment
		   (ad-compound-env procedure)
		   (ad-compound-formals procedure)
		   arguments)))
	(else
	 (error "Not an ad-procedure" procedure))))

(define (eval-ad-if form env)
  (let ((predicate (cadr form))
	(consequent (caddr form))
	(alternate (cadddr form)))
    (if (ad-eval predicate env)
	(ad-eval consequent env)
	(ad-eval alternate env))))

(define (eval-ad-lambda form env)
  (let ((formals (cadr form))
	(body (caddr form))) ; TODO Only one-form bodies in ad-lambda
    (make-ad-compound body env formals)))

(define (eval-ad-definition form env)
  (error "TODO definitions not supported in ad-eval yet"))

;;; Driver loop

(define ad-user-environment #f)

(define (run-ad)
  (if (not ad-user-environment)
      (set! ad-user-environment (make-ad-user-environment)))
  (display ";;; Ad> ")
  (let ((answer (perturbed-eval (read) ad-user-environment the-non-perturbation)))
    (display ";;; Ad value: ")
    (write answer)
    (newline))
  (run-ad))

(define (perturbed-eval form env perturbation-type)
  (cond ((symbol? form)
	 (perturbed-lookup form env perturbation-type))
	((pair? form)
	 (cond ((eq? (car form) 'quote)
		(scheme-value->perturbed-eval-value (cadr form) perturbation-type))
	       ((eq? (car form) 'lambda)
		(eval-ad-lambda form env)) ;; Really?
	       ((eq? (car form) 'define)
		(eval-perturbed-definition form env))
	       ((eq? (car form) 'if)
		(eval-perturbed-if form env perturbation-type))
	       (else
		(eval-perturbed-application form env perturbation-type))))
	(else
	 (scheme-value->perturbed-eval-value form perturbation-type))))

(define (eval-perturbed-application form env perturbation-type)
  (let ((procedure (perturbed-eval (car form) env perturbation-type))
	(arguments (map (lambda (subform)
			  (perturbed-eval subform env perturbation-type))
			(cdr form))))
    (perturbed-apply procedure arguments perturbation-type)))

(define (base-apply procedure arguments)
  (cond ((j*? (primal* procedure))
	 (apply-j* (car arguments) the-non-perturbation))
	((ad-primitive? procedure)
	 (apply (ad-primitive-implementation procedure) arguments))
	((ad-compound? procedure)
	 (perturbed-eval (ad-compound-body procedure)
			 (ad-extend-environment
			  (ad-compound-env procedure)
			  (ad-compound-formals procedure)
			  arguments)
			 the-non-perturbation))
	((perturbing-compound? procedure)
	 (let ((perturbation-type (perturbing-compound-perturbation-type procedure)))
	   (let ((make-dual (perturbation-type-dual perturbation-type))
		 (primal (perturbation-type-primal perturbation-type))
		 (perturbation (perturbation-type-perturbation perturbation-type)))
	     (let ((answer
		    (perturbed-apply (make-dual (perturbing-compound-base procedure) 0)
				     (list (make-dual (car arguments) (cadr arguments)))
				     perturbation-type)))
	       (cons (primal answer) (perturbation answer))))))
	(else
	 (error "Not an ad-procedure" procedure))))

(define (perturbed-apply procedure arguments perturbation-type)
  (if (non-perturbation? perturbation-type)
      (base-apply procedure arguments)
      (let ((make-dual (perturbation-type-dual perturbation-type))
	    (primal (perturbation-type-primal perturbation-type))
	    (perturbation (perturbation-type-perturbation perturbation-type)))
	(cond ((j*? (primal* procedure))
	       (apply-j* (car arguments) perturbation-type))
	      ((ad-primitive? (primal* procedure))
	       (make-dual
		(perturbed-apply
		 (primal procedure)
		 (map primal arguments)
		 (perturbation-type-parent perturbation-type))
		(do-multiply
		 (apply (jacobian (primal* procedure)) (map primal* arguments))
		 (map perturbation arguments))))
	      ((ad-compound? (primal* procedure))
	       (let ((procedure (primal* procedure)))
		 (perturbed-eval
		  (ad-compound-body procedure)
		  (ad-extend-environment
		   (ad-compound-env procedure)
		   (ad-compound-formals procedure)
		   arguments)
		  perturbation-type)))
	      ((perturbing-compound? procedure)
	       (let ((perturbation-type (perturbing-compound-perturbation-type procedure)))
		 (let ((make-dual (perturbation-type-dual perturbation-type))
		       (primal (perturbation-type-primal perturbation-type))
		       (perturbation (perturbation-type-perturbation perturbation-type)))
		   (let ((answer
			  (perturbed-apply (make-dual (perturbing-compound-base procedure) 0)
					   (list (make-dual (car arguments) (cadr arguments)))
					   perturbation-type)))
		     (cons (primal answer) (perturbation answer))))))
	      (else
	       (error "Not an ad-procedure" procedure))))))

(define (do-multiply J perturbation)
  (if (number? J)
      (if (not (= (length perturbation) 1))
	  (error "Size mismatch" J perturbation)
	  (* J (car perturbation)))
      (apply + (map * J perturbation))))

(define (eval-perturbed-if form env perturbation-type)
  (let ((predicate (cadr form))
	(consequent (caddr form))
	(alternate (cadddr form)))
    (if (primal* (perturbed-eval predicate env perturbation-type))
	(perturbed-eval consequent env perturbation-type)
	(perturbed-eval alternate env perturbation-type))))

(define (eval-perturbed-definition form env)
  (error "TODO definitions not supported in perturbed-eval yet"))

(define (scheme-value->perturbed-eval-value thing perturbation-type)
  (if (non-perturbation? perturbation-type)
      (scheme-value->ad-eval-value thing)
      ((perturbation-type-dual perturbation-type)
       (scheme-value->perturbed-eval-value thing (perturbation-type-parent perturbation-type))
       0)))

(define (perturbed-lookup symbol env perturbation-type)
  (if (non-perturbation? perturbation-type)
      (ad-lookup symbol env)
      (let ((make-dual (perturbation-type-dual perturbation-type))
	    (primal? (perturbation-type-primal? perturbation-type)))
	(let ((answer (perturbed-lookup symbol env (perturbation-type-parent perturbation-type))))
	  (if (primal? answer)
	      (make-dual answer 0)
	      answer)))))

(define (apply-j* procedure parent-perturbation-type)
  (make-perturbing-compound
   procedure
   (make-fresh-perturbation-type parent-perturbation-type)))

(define-structure perturbing-compound
  base
  perturbation-type)
