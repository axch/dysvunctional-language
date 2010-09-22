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
	   (if (non-perturbation? epsilon)
	       (apply (ad-primitive-implementation procedure) arguments)
	       (make-dual
		(perturbed-apply
		 (primal procedure)
		 (map primal arguments)
		 (epsilon-parent epsilon))
		(do-multiply
		 (apply (jacobian (primal* procedure)) (map primal* arguments))
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
  (if (number? J)
      (if (not (= (length perturbation) 1))
	  (error "Size mismatch" J perturbation)
	  (times-perturbation J (car perturbation)))
      (apply plus-perturbation (map times-perturbation J perturbation))))

(define (times-perturbation scalar perturbation)
  (cond ((number? perturbation)
	 (* scalar perturbation))
	((perturbed? perturbation)
	 ((gen-make-dual (perturbed-epsilon perturbation))
	  (times-perturbation scalar (perturbed-primal perturbation))
	  (times-perturbation scalar (perturbed-perturbation perturbation))))
	(else (error "Ack!  times-perturbation can't handle" scalar perturbation))))

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

(define (scheme-value->perturbed-eval-value thing epsilon)
  (scheme-value->ad-eval-value thing))

(define (perturbed-lookup symbol env epsilon)
  (ad-lookup symbol env))

(define (apply-j* procedure parent-epsilon)
  (make-perturbing-compound
   procedure
   (make-fresh-epsilon parent-epsilon)))

(define-structure perturbing-compound
  base
  epsilon)
