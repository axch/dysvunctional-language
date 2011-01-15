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
  (zero [self] (new closure (:formal self) (zero (:body self))
		    (map-env zero (:env self))))
  pair
  (zero [self] (new pair (zero (:car self)) (zero (:cdr self))))
  Number
  (zero [self] 0)

  variable
  (zero [self] self)
  constant
  (zero [self] (new constant (zero (:object self))))
  lambda-exp
  (zero [self] (new lambda-exp (:formal self) (zero (:body self))))
  application
  (zero [self] (new application (zero (:operator self)) (zero (:operand self))))
  empty-list
  (zero [self] self))

;;;; Syntax

(declare syntax-body syntax-operands syntax-formals macro? jl-expand-1)

(defn self-evaluating? [thing]
  (number? thing))

(defn syntax [exp]
  (cond (symbol? exp)
	(new variable exp)
	(self-evaluating? exp)
	(new constant exp)
	(macro? exp)
	(syntax (jl-expand-1 exp))
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

;;; Macros

(def macro-table {})

(defn macro? [exp]
  (and (seq? exp)
       (contains? macro-table (first exp))))

(defn jl-expand-1 [exp]
  ((get macro-table (first exp)) exp))

(defn jl-let [[_ bindings & body]]
  (cons (cons 'lambda (cons (map first bindings) body))
	(map second bindings)))

(def macro-table (assoc macro-table 'let jl-let))

;;;; Forward Mode

(declare forward-transform interleave-bundle interleave-bundle-deep)

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
	(new closure (:formal primal)
	     (interleave-bundle-deep (:body primal) (:body tangent))
	     (interleave-env (:env primal) (:env tangent)))
	(primitive? primal)
	primal))

(defn interleave-bundle-deep [primal tangent]
  (cond (variable? primal)
	;; TODO Should I be marking things and keeping track of
	;; perturbations here?
	primal
	(constant? primal)
	(new constant (make-bundle (:object primal) (:object tangent)))
	(lambda-exp? primal)
	(new lambda-exp (:formal primal)
	     (interleave-bundle-deep (:body primal) (:body tangent)))
	(application? primal)
	(new application (interleave-bundle-deep (:operator primal)
						 (:operator tangent))
	     (interleave-bundle-deep (:operand primal)
				     (:operand tangent)))
	(pair? primal)
	(new pair (interleave-bundle-deep (:car primal) (:car tangent))
	     (interleave-bundle-deep (:cdr primal) (:cdr tangent)))
	(empty-list? primal)
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
