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

(defprotocol Destructurable
  (jl-destructure [self arg]))

;;;; Evaluator

(declare jl-eval)

(defprotocol Applicable
  (jl-apply [self arg]))

(defrecord primitive [implementation]
  Applicable
  (jl-apply [self arg] (implementation arg)))

(defrecord closure [formal body env]
  Applicable
  (jl-apply [self arg]
   (jl-eval body (extend-env (jl-destructure formal arg) env))))

(defprotocol Evaluable
  (jl-eval [self env]))

(defrecord variable [name]
  Evaluable
  (jl-eval [self env] (lookup env name))
  Destructurable
  (jl-destructure [self arg] {name arg}))

(defrecord constant [object]
  Evaluable
  (jl-eval [self env] object)
  Destructurable
  (jl-destructure [self arg] {}))

(defrecord lambda-exp [formal body]
  Evaluable
  (jl-eval [self env] (new closure formal body env)))

(defrecord application [operator operand]
  Evaluable
  (jl-eval [self env] (jl-apply (jl-eval operator env)
				(jl-eval operand env))))

(defrecord pair [car cdr]
  Evaluable
  (jl-eval [self env] (new pair (jl-eval car env)
			   (jl-eval cdr env)))
  Destructurable
  (jl-destructure [self arg] (merge (jl-destructure car (:car arg))
				    (jl-destructure cdr (:cdr arg)))))

(defrecord empty-list [])

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

;;;; Initial environment

(defn binary-primitive [proc]
  (new primitive (fn [arg] (proc (:car arg) (:cdr arg)))))

(def inital-env {'+ (binary-primitive +)})

(jl-eval (syntax-body (sexp-slurp (clojure.java.io/file "bar.jl"))) inital-env)
