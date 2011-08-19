{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Optimize.Optimize where

import FOL.Language.AlphaRn
import FOL.Language.Common
import FOL.Language.Parser
import FOL.Language.Pretty
import FOL.Language.Unique

import FOL.Optimize.Inline

import Control.Monad

optimize :: String -> String
optimize = pprint . evalUnique . transform . parse
    where
      transform = inline <=< alphaRn

example = "(begin (define (operation-2 the-closure the-formals) (argument-types (vector) (vector) (cons real real)) (let () (operation-3 (vector) (vector)))) (define (operation-4 the-closure the-formals) (argument-types (vector (vector (vector real))) (vector (vector (vector real))) (vector (vector real) (vector (vector (vector (vector real)))))) (let ((recur the-formals)) (operation-7 (vector-ref the-closure 0) (vector recur)))) (define (operation-9 the-closure the-formals) (argument-types (vector) (cons (vector real) (cons real (vector))) real) (let ((f (car the-formals)) (start (car (cdr the-formals)))) (operation-11 (vector f start) (vector)))) (define (operation-13 the-closure the-formals) (argument-types (vector) (cons real real) bool) (let ((a (car the-formals)) (b (cdr the-formals))) (let ((temp-15 (cons (abs (let ((temp-14 (cons a b))) (- (car temp-14) (cdr temp-14)))) 0.00001))) (< (car temp-15) 0.00001)))) (define (operation-16 the-closure the-formals) (argument-types (vector (vector real) (vector (vector real) (vector (vector (vector (vector real))))) real) () real) (let () (operation-19 (vector-ref the-closure 1) (cons (vector-ref the-closure 2) (operation-18 (vector-ref the-closure 0) (vector-ref the-closure 2)))))) (define (operation-20 the-closure the-formals) (argument-types (vector (vector real) (vector (vector (vector (vector real)))) real) () real) (let () (operation-22 (vector-ref the-closure 1) (cons (vector-ref the-closure 2) (operation-18 (vector-ref the-closure 0) (vector-ref the-closure 2)))))) (define (operation-18 the-closure the-formals) (argument-types (vector real) real real) (let ((guess the-formals)) (let ((temp-25 (cons (let ((temp-24 (cons guess (let ((temp-23 (cons (vector-ref the-closure 0) guess))) (/ (car temp-23) (cdr temp-23)))))) (+ (car temp-24) (cdr temp-24))) 2))) (/ (car temp-25) 2)))) (define (operation-26 the-closure the-formals) (argument-types (vector) (vector) (cons real real)) (let () (operation-27 (vector) ()))) (define (operation-7 the-closure the-formals) (argument-types (vector (vector real)) (vector (vector (vector (vector real)))) (vector (vector real) (vector (vector (vector (vector real)))))) (let ((loop the-formals)) (vector (vector-ref the-closure 0) loop))) (define (operation-22 the-closure the-formals) (argument-types (vector (vector (vector (vector real)))) (cons real real) real) (let ((y the-formals)) (operation-19 (operation-4 (vector-ref the-closure 0) (vector-ref the-closure 0)) y))) (define (operation-27 the-closure the-formals) (argument-types (vector) () (cons real real)) (let () (cons 1.4142135623730951 (operation-29 (vector) (real 2))))) (define (operation-30 the-closure the-formals) (argument-types (vector) (vector (vector real)) (vector (vector real) (vector (vector (vector (vector real)))))) (let ((kernel the-formals)) (operation-4 (vector kernel) (vector kernel)))) (define (operation-11 the-closure the-formals) (argument-types (vector (vector real) real) (vector) real) (let () (operation-32 (vector (vector-ref the-closure 0) (vector-ref the-closure 1)) (operation-30 (vector) (vector (vector-ref the-closure 0)))))) (define (operation-33 the-closure the-formals) (argument-types (vector) real (vector real)) (let ((x the-formals)) (vector x))) (define (operation-32 the-closure the-formals) (argument-types (vector (vector real) real) (vector (vector real) (vector (vector (vector (vector real))))) real) (let ((loop the-formals)) (operation-16 (vector (vector-ref the-closure 0) loop (vector-ref the-closure 1)) ()))) (define (operation-1 the-closure the-formals) (argument-types (vector) () (cons real real)) (let () (operation-34 (vector) (vector)))) (define (operation-34 the-closure the-formals) (argument-types (vector) (vector) (cons real real)) (let () (operation-2 (vector) (vector)))) (define (operation-35 the-closure the-formals) (argument-types (vector real) () real) (let () (vector-ref the-closure 0))) (define (operation-3 the-closure the-formals) (argument-types (vector) (vector) (cons real real)) (let () (operation-26 (vector) (vector)))) (define (operation-29 the-closure the-formals) (argument-types (vector) real real) (let ((x the-formals)) (operation-9 (vector) (cons (operation-33 (vector) x) (cons (real 1.0) (vector)))))) (define (operation-19 the-closure the-formals) (argument-types (vector (vector real) (vector (vector (vector (vector real))))) (cons real real) real) (let ((old (car the-formals)) (new (cdr the-formals))) (if (operation-13 (vector) (cons old new)) (operation-35 (vector new) ()) (operation-20 (vector (vector-ref the-closure 0) (vector-ref the-closure 1) new) ())))) (operation-1 (vector) ()))"

example2 = "(begin (define (operation-2 the-closure the-formals) (argument-types (vector) (cons (vector) (cons real (cons real (cons real ())))) (cons real (cons real (cons real ())))) (let ((lst (cdr the-formals))) (operation-4 (vector lst) ()))) (define (operation-5 the-closure the-formals) (argument-types (vector (cons real (cons real ()))) () (cons real (cons real ()))) (let () (cons (operation-10 (vector) (operation-9 (vector) (vector-ref the-closure 0))) (operation-8 (vector) (cons (vector) (operation-7 (vector) (vector-ref the-closure 0))))))) (define (operation-11 the-closure the-formals) (argument-types (vector) (cons (vector) (cons real ())) (cons real ())) (let ((lst (cdr the-formals))) (operation-13 (vector lst) ()))) (define (operation-14 the-closure the-formals) (argument-types (vector) (cons (vector) (cons real (cons real ()))) (cons real (cons real ()))) (let ((y the-formals)) (operation-15 (vector) y))) (define (operation-16 the-closure the-formals) (argument-types (vector) () (cons (cons real (cons real (cons real ()))) (cons real (cons real ())))) (let () (cons (operation-2 (vector) (cons (vector) (cons (real 1) (cons (real 2) (cons (real 3) ()))))) (operation-17 (vector) (cons (vector) (cons (real 4) (cons (real 5) ()))))))) (define (operation-18 the-closure the-formals) (argument-types (vector) (cons real ()) real) (let ((x (car the-formals))) x)) (define (operation-19 the-closure the-formals) (argument-types (vector (cons real (cons real ()))) () (cons real (cons real ()))) (let () (cons (operation-22 (vector) (operation-9 (vector) (vector-ref the-closure 0))) (operation-21 (vector) (cons (vector) (operation-7 (vector) (vector-ref the-closure 0))))))) (define (operation-23 the-closure the-formals) (argument-types (vector) (cons real (cons real (cons real ()))) (cons real (cons real ()))) (let ((y (cdr the-formals))) y)) (define (operation-24 the-closure the-formals) (argument-types (vector) (cons real (cons real (cons real ()))) real) (let ((x (car the-formals))) x)) (define (operation-10 the-closure the-formals) (argument-types (vector) real real) (let ((x the-formals)) (let ((temp-25 (cons x 2))) (* (car temp-25) 2)))) (define (operation-22 the-closure the-formals) (argument-types (vector) real real) (let ((x the-formals)) (let ((temp-26 (cons x 1))) (+ (car temp-26) 1)))) (define (operation-8 the-closure the-formals) (argument-types (vector) (cons (vector) (cons real ())) (cons real ())) (let ((y the-formals)) (operation-27 (vector) y))) (define (operation-17 the-closure the-formals) (argument-types (vector) (cons (vector) (cons real (cons real ()))) (cons real (cons real ()))) (let ((lst (cdr the-formals))) (operation-5 (vector lst) ()))) (define (operation-28 the-closure the-formals) (argument-types (vector (cons real ())) () (cons real ())) (let () (cons (operation-10 (vector) (operation-18 (vector) (vector-ref the-closure 0))) ()))) (define (operation-21 the-closure the-formals) (argument-types (vector) (cons (vector) (cons real ())) (cons real ())) (let ((y the-formals)) (operation-11 (vector) y))) (define (operation-15 the-closure the-formals) (argument-types (vector) (cons (vector) (cons real (cons real ()))) (cons real (cons real ()))) (let ((lst (cdr the-formals))) (operation-19 (vector lst) ()))) (define (operation-30 the-closure the-formals) (argument-types (vector) (vector) (cons (cons real (cons real (cons real ()))) (cons real (cons real ())))) (let () (operation-31 (vector) (vector)))) (define (operation-1 the-closure the-formals) (argument-types (vector) (cons (vector) (cons (vector) (cons (vector) (vector)))) (cons (cons real (cons real (cons real ()))) (cons real (cons real ())))) (let () (operation-30 (vector) (vector)))) (define (operation-13 the-closure the-formals) (argument-types (vector (cons real ())) () (cons real ())) (let () (cons (operation-22 (vector) (operation-18 (vector) (vector-ref the-closure 0))) ()))) (define (operation-4 the-closure the-formals) (argument-types (vector (cons real (cons real (cons real ())))) () (cons real (cons real (cons real ())))) (let () (cons (operation-22 (vector) (operation-24 (vector) (vector-ref the-closure 0))) (operation-14 (vector) (cons (vector) (operation-23 (vector) (vector-ref the-closure 0))))))) (define (operation-7 the-closure the-formals) (argument-types (vector) (cons real (cons real ())) (cons real ())) (let ((y (cdr the-formals))) y)) (define (operation-9 the-closure the-formals) (argument-types (vector) (cons real (cons real ())) real) (let ((x (car the-formals))) x)) (define (operation-31 the-closure the-formals) (argument-types (vector) (vector) (cons (cons real (cons real (cons real ()))) (cons real (cons real ())))) (let () (operation-16 (vector) ()))) (define (operation-27 the-closure the-formals) (argument-types (vector) (cons (vector) (cons real ())) (cons real ())) (let ((lst (cdr the-formals))) (operation-28 (vector lst) ()))) (operation-1 (vector) (cons (vector) (cons (vector) (cons (vector) (vector))))))"

example3 = "(begin (define (foo a b c d e f g h) (argument-types real (cons real real) (cons real (cons real real)) (cons real (cons real (cons real real))) (cons real (cons real (cons real (cons real real)))) bool () (vector) real) (bar a b)) (define (bar x y) (argument-types real (cons real real) real) (+ x (car y))) (bar 1 (cons 2 3)))"
