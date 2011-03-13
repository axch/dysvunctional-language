(declare (usual-integrations))
;;;; Post processing

;;; The post-processing stage consists of several sub-stages.  They
;;; need to be done in order, but you can invoke any subsequence to
;;; see the effect of doing only that level of post-processing.
;;; We have:
;;; - STRUCTURE-DEFINITIONS->VECTORS
;;;   Replace DEFINE-STRUCTURE with explicit vectors.
;;; - INLINE
;;;   Inline non-recursive function definitions.
;;; - SCALAR-REPLACE-AGGREGATES
;;;   Replace aggregates with scalars at procedure boundaries.
;;;   This relies on argument-type annotations being emitted by the
;;;   code generator.
;;; - STRIP-ARGUMENT-TYPES
;;;   Remove argument-type annotations, if they have been emitted by
;;;   the code generator (because SCALAR-REPLACE-AGGREGATES is the
;;;   only thing that needs them).
;;; - TIDY
;;;   Clean up and optimize locally by term-rewriting.

(define (prettify-compiler-output output)
  (if (list? output)
      ((lambda (x) x) ; This makes tidying show up in the stack sampler
       (tidy
        (full-alpha-rename
         (strip-argument-types
          (scalar-replace-aggregates
           (inline
            (structure-definitions->vectors
             output)))))))
      output))

(define (compile-to-scheme program)
  (prettify-compiler-output
   (analyze-and-generate-with-type-declarations program)))

;;; Don't worry about the rule-based term-rewriting system that powers
;;; this.  That is its own pile of stuff, good for a few lectures of
;;; Sussman's MIT class Adventures in Advanced Symbolic Programming.
;;; It works, and it's very good for peephole manipulations of
;;; structured expressions (like the output of the VL code generator).
;;; If you really want to see it, though, it's included in
;;; support/rule-system.

;;; Rules for the term-rewriting system consist of a pattern to try to
;;; match and an expression to evaluate to compute a replacement for
;;; that match should a match be found.  Patterns match themselves;
;;; the construct (? name) introduces a pattern variable named name;
;;; the construct (? name ,predicate) is a restricted pattern variable
;;; which only matches things the predicate accepts; the construct (??
;;; name) introduces a sublist pattern variable.  The pattern matcher
;;; will search through possible lengths of sublists to find a match.
;;; Repeated pattern variables must match equal structures in all the
;;; corresponding places.

;;; A rule by itself is a one-argument procedure that tries to match
;;; its pattern.  If the match succeeds, the rule will evaluate the
;;; the replacement expression in an environment where the pattern
;;; variables are bound to the things they matched and return the
;;; result.  If the replacement expression returns #f, that tells the
;;; matcher to backtrack and look for another match.  If the match
;;; fails, the rule will return #f.

;;; A rule simplifier has a set of rules, and applies them to every
;;; subexpression of the input expression repeatedly until the result
;;; settles down.

;;;; Turning record structures into vectors

;;; Just replace every occurrence of DEFINE-STRUCTURE with the
;;; corresponding pile of vector operations.  Also need to make sure
;;; that the argument types declarations, if any, all say VECTOR
;;; rather than whatever the name of the structure used to be.

(define (structure-definition? form)
  (and (pair? form)
       (eq? (car form) 'define-structure)))

(define (expand-if-structure-definition form)
  (if (structure-definition? form)
      (let ((name (cadr form))
            (fields (cddr form)))
        `((define ,(symbol 'make- name) vector)
          ,@(map (lambda (field index)
                   `(define (,(symbol name '- field) thing)
                      (vector-ref thing ,index)))
                 fields
                 (iota (length fields)))))
      (list form)))

(define (structure-definitions->vectors forms)
  (define (hash-table/put-alist! table alist)
    (for-each (lambda (k.v)
                (hash-table/put! table (car k.v) (cdr k.v)))
              alist))
  (define (alist->eq-hash-table alist)
    (let ((answer (make-eq-hash-table)))
      (hash-table/put-alist! answer alist)
      answer))
  (let* ((structure-names
          (map cadr (filter structure-definition? forms)))
         (structure-name-map
          (alist->eq-hash-table
           (map (lambda (name) (cons name #t)) structure-names))))
    (define (structure-name? name)
      (hash-table/get structure-name-map name #f))
    (define structure-names->vectors
      (on-subexpressions
       (rule `(? type ,structure-name?) 'vector)))
    (define fix-argument-types
      (rule `(define (? formals)
               (argument-types (?? arg-types))
               (?? body))
            `(define ,formals
               (argument-types ,@(structure-names->vectors arg-types))
               ,@body)))
    (map fix-argument-types
         (append-map expand-if-structure-definition forms))))

;;;; Scalar replacement of aggregates

;;; If some procedure accepts a structured argument, it can be
;;; converted into accepting the fields of that argument instead, as
;;; long as all the call sites are changed to pass the fields instead
;;; of the structure at the same time.  This piece of code does this
;;; in a way that is local to the definitions and call sites --- the
;;; new procedure definition just reconstructs the structure from the
;;; passed arguments, and the new call sites just extract the fields
;;; from the structure they would have passed.  However, once this
;;; tranformation is done, further local simplifications done by TIDY
;;; will have the effect of eliminating those structures completely.

;;; This process relies on the code generator having emitted argument
;;; type declarations.  If there are no argument type declarations,
;;; nothing will happen.

;;; The key trick in how this is done is SRA-DEFINITION-RULE, which
;;; pattern matches on a definition with an argument type declaration,
;;; and, if the definition accepted a structured argument, returns a
;;; rewritten definition and a rule for transforming the call sites.

(define (cons-or-vector? thing)
  (or (eq? thing 'cons)
      (eq? thing 'vector)))

(define sra-definition-rule
  (rule
   `(define ((? name) (?? formals1) (? formal) (?? formals2))
      (argument-types
       (?? stuff1)
       ((? formal) ((? constructor ,cons-or-vector?)
                    (?? slot-shapes)))
       (?? stuff2))
      (?? body))
   (let ((slot-names (map (lambda (shape)
                            (make-name (symbol formal '-)))
                          slot-shapes))
         (arg-index (length formals1))
         (num-slots (length slot-shapes))
         (arg-count (+ (length formals1) 1 (length formals2))))
     (cons (sra-call-site-rule
            name constructor arg-index num-slots arg-count)
           `(define (,name ,@formals1 ,@slot-names ,@formals2)
              (argument-types
               ,@stuff1
               ,@(map list slot-names slot-shapes)
               ,@stuff2)
              (let ((,formal (,constructor ,@slot-names)))
                ,@body))))))

(define (sra-call-site-rule
         operation-name constructor arg-index num-slots arg-count)
  (rule
   `(,operation-name (?? args))
   (and (= (length args) arg-count)
        (let ((args1 (take args arg-index))
              (arg (list-ref args arg-index))
              (args2 (drop args (+ arg-index 1)))
              (temp-name (make-name 'temp-)))
          `(let ((,temp-name ,arg))
             (,operation-name
              ,@args1
              ,@(call-site-replacement temp-name constructor num-slots)
              ,@args2))))))

(define (call-site-replacement temp-name constructor-type count)
  (if (eq? 'cons constructor-type)
      `((car ,temp-name) (cdr ,temp-name))
      (map (lambda (index)
             `(vector-ref ,temp-name ,index))
           (iota count))))

;;; The actual SCALAR-REPLACE-AGGREGATES procedure just tries
;;; SRA-DEFINITION-RULE on all the possible definitions as many times
;;; as it does something.  Whenever SRA-DEFINITION-RULE rewrites a
;;; definition, SCALAR-REPLACE-AGGREGATES applies the resulting
;;; sra-call-site-rule to rewrite all the call sites.  The only tricky
;;; bit is to make sure not to apply the sra-call-site-rule to the
;;; formal parameter list of the definition just rewritten, because it
;;; will match it and screw it up.

(define (scalar-replace-aggregates forms)
  (define (do-sra-definition sra-result done rest)
    (let ((sra-call-site-rule (car sra-result))
          (replacement-form (cdr sra-result)))
      (let ((sra-call-sites (on-subexpressions sra-call-site-rule)))
        (let ((fixed-replacement-form
               `(,(car replacement-form) ,(cadr replacement-form)
                 ,(caddr replacement-form)
                 ,(sra-call-sites (cadddr replacement-form))))
              (fixed-done (sra-call-sites (reverse done)))
              (fixed-rest (sra-call-sites rest)))
          (append fixed-done (list fixed-replacement-form) fixed-rest)))))
  (let loop ((forms forms))
    (let scan ((done '()) (forms forms))
      (if (null? forms)
          (reverse done)
          (let ((sra-attempt (sra-definition-rule (car forms))))
            (if (not (eq? (car forms) sra-attempt))
                (loop (do-sra-definition sra-attempt done (cdr forms)))
                (scan (cons (car forms) done) (cdr forms))))))))

;;; Getting rid the argument-types declarations once we're done with
;;; them is easy.

(define remove-defn-argument-types
  (rule `(define (? formals)
           (argument-types (?? etc))
           (?? body))
        `(define ,formals
           ,@body)))

(define strip-argument-types
  (rule-simplifier
   (list
    (rule `(begin (define-syntax argument-types (?? etc))
                  (?? stuff))
          `(begin
             ,@stuff))
    remove-defn-argument-types)))
;;;; Inlining procedure definitions

;;; Every procedure that does not call itself can be inlined.  To do
;;; that, just replace calls to that procedure with the corresponding
;;; LET form.

(define (inline forms)
  (define (non-self-calling? defn)
    (not (occurs-in-tree? (definiendum defn) (definiens defn))))
  (define (inline-defn defn forms)
    (let ((defn (remove-defn-argument-types defn)))
      (let ((name (definiendum defn))
            (replacement (definiens defn)))
        ((on-subexpressions
          (rule `(,name (?? args))
                (->let `(,replacement ,@args))))
         forms))))
  (let loop ((forms forms))
    (let scan ((done '()) (forms forms))
      (cond ((null? forms) (reverse done))
            ((and (definition? (car forms))
                  (non-self-calling? (car forms)))
             (let ((defn (car forms))
                   (others (append (reverse done) (cdr forms))))
               ;; Can insert other inlining restrictions here
               (loop (inline-defn defn others))))
            (else (scan (cons (car forms) done) (cdr forms)))))))

(define (inline forms)
  (define (hash-table/put-alist! table alist)
    (for-each (lambda (k.v)
                (hash-table/put! table (car k.v) (cdr k.v)))
              alist))
  (define (alist->eq-hash-table alist)
    (let ((answer (make-eq-hash-table)))
      (hash-table/put-alist! answer alist)
      answer))
  (let* ((definitions (filter definition? forms))
         (defn-map (alist->eq-hash-table
                    (map (lambda (defn)
                           (cons (definiendum defn) defn))
                         definitions)))
         (call-graph
          (map cons definitions
               (map (lambda (defn)
                      (delete-duplicates
                       (filter-map-tree (lambda (leaf)
                                          (hash-table/get defn-map leaf #f))
                                        (definiens defn))
                       eq?))
                    definitions)))
         (non-inlinees (feedback-vertex-set call-graph))
         (inlinees (lset-difference eq? definitions non-inlinees))
         (inline-alist
          (map cons (map definiendum inlinees)
               (map definiens (map remove-defn-argument-types inlinees))))
         (inline-map (alist->eq-hash-table inline-alist)))
    (define (inline? name)
      (hash-table/get inline-map name #f))
    (define (not-inline? form)
      (or (not (definition? form))
          (not (inline? (definiendum form)))))
    (let walk ((program (filter not-inline? forms)))
      ((on-subexpressions
        (rule `((? name ,inline?) (?? args))
              (walk (->let `(,(hash-table/get inline-map name #f) ,@args)))))
       program))))

(define (full-alpha-rename program)
  ;; TODO Fix the bookkeeping of what names the primitives rely on
  (define (needed-names primitive)
    (list (primitive-name primitive)))
  (alpha-rename program
   (map (lambda (name)
          (cons name name))
        (delete-duplicates
         `(cons car cdr if define let vector vector-ref
                ,@(append-map needed-names *primitives*))))))

;;;; Term-rewriting tidier

(define intraprocedural-sra-rule
  (rule `(let ((?? bindings1)
               ((? name ,symbol?) ((? constructor ,cons-or-vector?) (?? args)))
               (?? bindings2))
           (?? body))
        (let ((slot-names (map (lambda (arg)
                                 (make-name (symbol name '-)))
                               args)))
          `(let (,@bindings1
                 ,@(map list slot-names args)
                 ,@bindings2)
             ,@(replace-free-occurrences name `(,constructor ,@slot-names) body)))))

(define intraprocedural-variable-elimination-rule
  (rule `(let ((?? bindings1)
               ((? name ,symbol?) (? exp))
               (?? bindings2))
           (?? body))
        (let ((occurrence-count (count-free-occurrences name body)))
          (and (or (= 0 occurrence-count)
                   (= 1 occurrence-count)
                   (constructors-only? exp))
               `(let (,@bindings1
                      ,@bindings2)
                  ,@(replace-free-occurrences name exp body))))))

(define tidy
  (rule-simplifier
   (list
    (rule `(let () (? body)) body)
    (rule `(begin (? body)) body)
    (rule `(car (cons (? a) (? d))) a)
    (rule `(cdr (cons (? a) (? d))) d)
    (rule `(vector-ref (vector (?? stuff)) (? index ,integer?))
          (list-ref stuff index))

    (rule `(car (let (? bindings) (? body)))
          `(let ,bindings (car ,body)))
    (rule `(cdr (let (? bindings) (? body)))
          `(let ,bindings (cdr ,body)))
    (rule `(vector-ref (let (? bindings) (? body)) (? index ,integer?))
          `(let ,bindings (vector-ref ,body ,index)))

    (rule `(* 0 (? thing)) 0)
    (rule `(* (? thing) 0) 0)
    (rule `(+ 0 (? thing)) thing)
    (rule `(+ (? thing) 0) thing)
    (rule `(* 1 (? thing)) thing)
    (rule `(* (? thing) 1) thing)

    intraprocedural-variable-elimination-rule
    intraprocedural-sra-rule)))

(define tidy
  (iterated
   (in-order
    (top-down
     (rule-list
      (list
       (rule `(let () (? body)) body)
       (rule `(begin (? body)) body)
       (rule `(car (cons (? a) (? d))) a)
       (rule `(cdr (cons (? a) (? d))) d)
       (rule `(vector-ref (vector (?? stuff)) (? index ,integer?))
             (list-ref stuff index))

       (rule `(car (let (? bindings) (? body)))
             `(let ,bindings (car ,body)))
       (rule `(cdr (let (? bindings) (? body)))
             `(let ,bindings (cdr ,body)))
       (rule `(vector-ref (let (? bindings) (? body)) (? index ,integer?))
             `(let ,bindings (vector-ref ,body ,index)))

       (rule `(* 0 (? thing)) 0)
       (rule `(* (? thing) 0) 0)
       (rule `(+ 0 (? thing)) thing)
       (rule `(+ (? thing) 0) thing)
       (rule `(* 1 (? thing)) thing)
       (rule `(* (? thing) 1) thing))))
    (on-subexpressions intraprocedural-variable-elimination-rule)
    (on-subexpressions intraprocedural-sra-rule))))
