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

(defprotocol JLObject
  (variable? [self])
  (constant? [self])
  (primitive? [self])
  (closure? [self])
  (lambda-exp? [self])
  (application? [self])
  (pair? [self])
  (empty-list? [self]))

(extend-type Object
  JLObject
  (variable? [self] false)
  (constant? [self] false)
  (primitive? [self] false)
  (closure? [self] false)
  (lambda-exp? [self] false)
  (application? [self] false)
  (pair? [self] false)
  (empty-list? [self] false))

(declare jl-eval)

(defprotocol Zeroable
  (zero [self]))

(defprotocol Applicable
  (jl-apply [self arg]))

(defrecord primitive [implementation]
  JLObject
  (primitive? [self] true)
  Applicable
  (jl-apply [self arg] (implementation arg))
  Zeroable
  (zero [self] self))

(defrecord closure [formal body env]
  JLObject
  (closure? [self] true)
  Applicable
  (jl-apply [self arg]
	    (jl-eval body (extend-env (jl-destructure formal arg) env)))
  Zeroable
  ;; TODO zero out the constants too
  (zero [self] (new closure formal body (map-env zero env))))

(defprotocol Evaluable
  (jl-eval [self env]))

(defrecord variable [name]
  JLObject
  (variable? [self] true)
  Evaluable
  (jl-eval [self env] (lookup env name))
  Destructurable
  (jl-destructure [self arg] {name arg}))

(defrecord constant [object]
  JLObject
  (constant? [self] true)
  Evaluable
  (jl-eval [self env] object)
  Destructurable
  (jl-destructure [self arg] {}))

(defrecord lambda-exp [formal body]
  JLObject
  (lambda-exp? [self] true)
  Evaluable
  (jl-eval [self env] (new closure formal body env)))

(defrecord application [operator operand]
  JLObject
  (application? [self] true)
  Evaluable
  (jl-eval [self env] (jl-apply (jl-eval operator env)
				(jl-eval operand env))))

(defrecord pair [car cdr]
  JLObject
  (pair? [self] true)
  Evaluable
  (jl-eval [self env] (new pair (jl-eval car env)
			   (jl-eval cdr env)))
  Destructurable
  (jl-destructure [self arg] (merge (jl-destructure car (:car arg))
				    (jl-destructure cdr (:cdr arg))))
  Zeroable
  (zero [self] (new pair (zero car) (zero cdr))))

(defrecord empty-list []
  JLObject
  (empty-list? [self] true))

(extend-type Number
  Zeroable
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

(declare forward-transform)

(defprotocol Interleavable
  (interleave-bundle [primal tangent]))

(defrecord bundle [primal tangent]
  Applicable
  (jl-apply [self arg] (jl-apply (forward-transform (interleave-bundle primal tangent))
				 arg)))

(defn make-bundle [primal tangent]
  (new bundle primal tangent))

(extend-type pair
  Interleavable
  (interleave-bundle [primal tangent] (new pair (make-bundle (:car primal) (:car tangent))
					   (make-bundle (:cdr primal) (:cdr tangent)))))

(extend-type closure
  Interleavable
  ;; TODO Get the constants from the body of the tangent
  (interleave-bundle
   [primal tangent]
   (new closure (:formal primal) (:body primal)
	(interleave-env (:env primal) (:env tangent)))))

(extend-type primitive
  Interleavable
  (interleave-bundle [primal tangent] primal))

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

(jl-eval (syntax-body (sexp-slurp (clojure.java.io/file "foo.jlad"))) inital-env)
