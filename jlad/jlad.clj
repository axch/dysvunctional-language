(defn sexp-slurp [file]
  (let [reader (new java.io.PushbackReader (clojure.java.io/reader file))
	eof-object (list 'eof-object)]
    ((fn step []
       (lazy-seq
	(let [next (read reader false eof-object)]
	  (if (identical? next eof-object)
	    nil
	    (cons next (step)))))))))

;;;; Environments

;;; An environment is just a Clojure map from names to values

(defn lookup [env var]
  (let [answer (get env var)]
    (if answer
      answer
      (throw (new IllegalArgumentException)))))

(defn extend-env [new-bindings env]
  (merge env new-bindings))

(defn map-env [f env]
  (zipmap (keys env) (map f (vals env))))

(declare make-bundle)

(defn interleave-env [primal-env tangent-env]
  (merge-with make-bundle primal-env tangent-env))

(defprotocol Destructurable
  (jl-destructure [self arg]))

;;;; Evaluator

(defprotocol JLVariable
  (variable? [self]))

(defprotocol JLConstant
  (constant? [self]))

(defprotocol JLPrimitive
  (primitive? [self]))

(defprotocol JLClosure
  (closure? [self]))

(defprotocol JLLambdaExp
  (lambda-exp? [self]))

(defprotocol JLApplication
  (application? [self]))

(defprotocol JLPair
  (pair? [self]))

(defprotocol JLEmptyList
 (empty-list? [self]))

(extend-type Object
  JLVariable
  (variable? [self] false)
  JLConstant
  (constant? [self] false)
  JLPrimitive
  (primitive? [self] false)
  JLClosure
  (closure? [self] false)
  JLLambdaExp
  (lambda-exp? [self] false)
  JLApplication
  (application? [self] false)
  JLPair
  (pair? [self] false)
  JLEmptyList
  (empty-list? [self] false))

(declare jl-eval)

(defprotocol Applicable
  (jl-apply [self arg]))

(defrecord primitive [implementation]
  JLPrimitive
  (primitive? [self] true)
  Applicable
  (jl-apply [self arg] (implementation arg)))

(defrecord closure [formal body env]
  JLClosure
  (closure? [self] true)
  Applicable
  (jl-apply [self arg]
	    (jl-eval body (extend-env (jl-destructure formal arg) env))))

(defprotocol Evaluable
  (jl-eval [self env]))

(defrecord variable [name]
  JLVariable
  (variable? [self] true)
  Evaluable
  (jl-eval [self env] (lookup env name))
  Destructurable
  (jl-destructure [self arg] {name arg}))

(defrecord constant [object]
  JLConstant
  (constant? [self] true)
  Evaluable
  (jl-eval [self env] object)
  Destructurable
  (jl-destructure [self arg] {}))

(defrecord lambda-exp [formal body]
  JLLambdaExp
  (lambda-exp? [self] true)
  Evaluable
  (jl-eval [self env] (new closure formal body env)))

(defrecord application [operator operand]
  JLApplication
  (application? [self] true)
  Evaluable
  (jl-eval [self env] (jl-apply (jl-eval operator env)
				(jl-eval operand env))))

(defrecord pair [car cdr]
  JLPair
  (pair? [self] true)
  Evaluable
  (jl-eval [self env] (new pair (jl-eval car env)
			   (jl-eval cdr env)))
  Destructurable
  (jl-destructure [self arg] (merge (jl-destructure car (:car arg))
				    (jl-destructure cdr (:cdr arg)))))

(defrecord empty-list []
  JLEmptyList
  (empty-list? [self] true))

(defprotocol Zeroable
  (zero [self]))

(extend-protocol Zeroable
  primitive
  (zero [self] self)
  closure
  ;; TODO zero out the constants too
  (zero [self] (new closure (:formal self) (:body self)
		    (map-env zero (:env self))))
  pair
  (zero [self] (new pair (zero (:car self)) (zero (:cdr self))))
  Number
  (zero [self] 0))

;;;; Syntax

(declare syntax-body syntax-operands syntax-formals)

(defn self-evaluating? [thing]
  (number? thing))

(defn syntax [exp]
  (cond (symbol? exp)
	(new variable exp)
	(self-evaluating? exp)
	(new constant exp)
	(seq? exp)
	(condp = (first exp)
	    'lambda (new lambda-exp
			 (syntax-formals (second exp))
			 (syntax-body (nnext exp)))
	    'cons (new pair (syntax (second exp))
		       (syntax (nth exp 2)))
	    (new application (syntax (first exp))
		 (syntax-operands (next exp))))))

(defn syntax-body [exps]
  ;; TODO Only one-form bodies for now
  (syntax (first exps)))

(defn syntax-operands [exps]
  (cond (empty? exps)
	(new constant (new empty-list))
	(empty? (rest exps))
	(syntax (first exps))
	true
	(new pair (syntax (first exps))
	     (syntax-operands (next exps)))))

(def syntax-formals syntax-operands)

;;;; Forward Mode

(declare forward-transform interleave-bundle)

(defrecord bundle [primal tangent]
  Applicable
  (jl-apply [self arg]
   (jl-apply (forward-transform (interleave-bundle primal tangent)) arg)))

(defn make-bundle [primal tangent]
  (new bundle primal tangent))

(defn interleave-bundle [primal tangent]
  (cond (pair? primal)
	(new pair (make-bundle (:car primal) (:car tangent))
	     (make-bundle (:cdr primal) (:cdr tangent)))
	(closure? primal)
	;; TODO Get the constants from the body of the tangent
	(new closure (:formal primal) (:body primal)
	     (interleave-env (:env primal) (:env tangent)))
	(primitive? primal)
	primal))

(defn forward-transform [thing]
  (if (contains? (meta thing) :forward)
    (:forward (meta thing))
    ;; TODO Do I actually need to do anything here?  Like count how
    ;; many transforms deep we are, so as to avoid perturbation
    ;; confusion?
    thing))

;;;; Initial environment

(defn with-forward-transform [primal forward]
  (with-meta primal (assoc (meta primal) :forward forward)))

(defn binary-primitive [proc]
  (new primitive (fn [arg] (proc (:car arg) (:cdr arg)))))

(def inital-env {'+ (binary-primitive +)
		 'zero (new primitive zero)
		 'bundle (binary-primitive make-bundle)
		 'primal (new primitive :primal)
		 'tangent (new primitive :tangent)})

(defn jl-do [form]
  (jl-eval (syntax form) inital-env))

; (jl-eval (syntax-body (sexp-slurp (clojure.java.io/file "foo.jlad"))) inital-env)
