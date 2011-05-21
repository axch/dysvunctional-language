(define (force stream)
  (stream))

(define (head stream)
  (car (force stream)))

(define (tail stream)
  (cdr (force stream)))

(define (nth-stream stream k)
  (let (((cons first rest) (force stream)))
    (if (<= k 0)
        first
        (nth-stream rest (- k 1)))))

(define (ones)
  (cons 1 ones))

; (nth-stream ones (real 4))

(define (stream-of-iterates next value)
  (lambda ()
    (cons value (stream-of-iterates next (next value)))))

(define (increment x)
  (+ x 1))

(define integers (stream-of-iterates increment (real 0)))

; Constrast (nth-stream integers 6)
; with (nth-stream integers (real 6))

(define (map-streams1 f s1)
  (lambda ()
    (let (((cons s1-first s1-rest) (force s1)))
      (cons (f s1-first) (map-streams1 f s1-rest)))))

(define (map-streams f s1 s2)
  (lambda ()
    (let (((cons s1-first s1-rest) (force s1))
          ((cons s2-first s2-rest) (force s2)))
      (cons (f s1-first s2-first)
            (map-streams f s1-rest s2-rest)))))

(define (stream-of-adjacent-pairs stream)
  (let (((cons first rest) (force stream)))
    (let loop ((first first) (rest rest))
      (lambda ()
        (let (((cons second rest) (force rest)))
          (cons (cons first second)
                (loop second rest)))))))

(define (refine-by-doubling s)
  (/ s (sqrt (+ 2 (sqrt (- 4 (* s s)))))))

(define side-lengths (stream-of-iterates refine-by-doubling (real (sqrt 2))))

(define side-numbers (stream-of-iterates (lambda (n) (* 2 n)) (real 4)))

(define (semi-perimeter length-of-side number-of-sides)
  (* (/ number-of-sides 2) length-of-side))

(define archimedian-pi-sequence
  (map-streams semi-perimeter side-lengths side-numbers))

; (nth-stream archimedian-pi-sequence (real 19))

(define (make-zeno-sequence R h)
  (lambda ()
    (cons (R h)
          (make-zeno-sequence R (/ h 2)))))

(define (richardson-trick error-order)
  (let ((2^p (expt 2 error-order)))
    (lambda (Rh Rh/2)
      (/ (- (* 2^p Rh/2) Rh) (- 2^p 1)))))

;; This version duplicates the work of computing the stream values
#;
(define (accelerate-zeno-sequence seq error-order)
  (map-streams
   (richardson-trick error-order)
   seq
   (tail seq)))

;; This version therefore compiles much faster.
;; I think I would need cross-loop-iteration alias analysis to
;; recollapse the computations.
(define (accelerate-zeno-sequence seq error-order)
  (map-streams1 (richardson-trick error-order) (stream-of-adjacent-pairs seq)))

; (nth-stream (accelerate-zeno-sequence archimedian-pi-sequence (real 2)) (real 1))

;;; Hm.  Unfortunately, I can't actually do this.  The problem is that
;;; the analysis eagerly chases down the definition of the stream, but
;;; each new element is made by a closure that has a longer chain of
;;; accelerations in its environment.  I suppose I should even have
;;; known that: computing each next element of the tableau requires an
;;; increasing amount of intermediate storage, so it can't be
;;; union-free.
(define (make-richardson-tableau seq error-orders)
  (lambda ()
    (let (((cons order rest) (force error-orders)))
      (cons seq (make-richardson-tableau
                 (accelerate-zeno-sequence seq order)
                 rest)))))

(define (richardson-sequence seq error-orders)
  (map-streams1 head (make-richardson-tableau seq error-orders)))

(define evens (stream-of-iterates (lambda (n) (+ n 2)) (real 2)))

(nth-stream (richardson-sequence archimedian-pi-sequence evens) (real 0))

