;;; How to fix the amazing bug?

(define (tangent epsilon thing)
  (cond ((procedure? thing)
         (hide-gensym-in-procedure epsilon
          (lambda (x)
            (tangent epsilon (thing x)))))
        ...))

;;; This version isn't going to work.  Consider
;;; f:: R -> R -> (R->R) -> (R->R)
;;; Then ((d f) x1) is protected by this hiding,
;;; but (((d f) x1) x2) is not; only by the swap, which is now
;;; deterministic.  It will be vulnerable to confusion on the new gensym,
;;; because it will be emitting and accepting closures that contain it.
(define (hide-gensym-in-procedure symbol procedure)
  (lambda (x)
    ((swap-gensyms-in-object procedure symbol (gensym)) x)))

;;; This version should work.  Why?  The NEW-SYMBOL is made after I
;;; see the input, so the input cannot contain it.  This means that
;;; the input cannot tickle any instance of SYMBOL in PROCEDURE.  So
;;; far so good.  But is there any way for SYMBOL to escape PROCEDURE?
;;; If it tries, it will come out looking like NEW-SYMBOL, so I just
;;; re-hide that.

;;; Then again, maybe not.  What if X contains some procedure, and
;;; PROCEDURE calls it with several different closure arguments all of
;;; which are closed over SYMBOL?  Then the procedure in X will see
;;; several different closures with the same gensym in them, and that
;;; are no longer hiding anything, and may smash them into each other
;;; can cause chaos.  Would that even be wrong?
(define (hide-gensym-in-procedure symbol procedure)
  (lambda (x)
    ;; Can I afford to bind the gensym outside the binder?  What
    ;; invariants would I need for that to work?  Do I even need to do
    ;; it for anything?
    (let* ((new-symbol (gensym))
           (answer ((swap-gensyms-in-object procedure symbol new-symbol) x)))
      (hide-gensym-in-object new-symbol answer))))

;;; I hope that the use pattern will be such that the symbol I am
;;; trying to hide only occurs inside closures, never exposed.
(define (hide-gensym-in-object symbol object)
  (cond ((procedure? object)
         (hide-gensym-in-procedure symbol object))
        ((pair? object)
         (cons (hide-gensym-in-object symbol (car object))
               (hide-gensym-in-object symbol (cdr object))))
        ;; Can I rely on the object never *being* the symbol I am hiding?
        ;; What should I do if it is?
        ((and (gensym? object) (gensym= symbol object))
         (gensym))
        (else object)))

(define (swap-gensyms-in-object object symbol new-symbol)
  (cond ((procedure? object)
         (lambda (x)
           (let* ((new-x (swap-gensyms-in-object x symbol new-symbol))
                  (answer (object new-x)))
             (swap-gensyms-in-object answer symbol new-symbol))))
        ((gensym? object)
         (if (gensym= object symbol)
             new-symbol
             (if (gensym= object new-symbol)
                 symbol
                 object)))
        ((pair? object)
         (cons (swap-gensyms-in-object (car object) symbol new-symbol)
               (swap-gensyms-in-object (cdr object) symbol new-symbol)))
        (else object)))
