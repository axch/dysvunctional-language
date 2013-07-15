;;; ----------------------------------------------------------------------
;;; Copyright 2013 Alexey Radul.
;;; ----------------------------------------------------------------------
;;; This file is part of DysVunctional Language.
;;; 
;;; DysVunctional Language is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;;  License, or (at your option) any later version.
;;; 
;;; DysVunctional Language is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Affero General Public License
;;; along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

(declare (usual-integrations))

;;;; Abstraction of the concept of a "backend"

;;; To facilitate selecting them dynamically, and checking whether a
;;; given operation is possible.

;;; The interface of a backend is as follows:
;;; - The name is an identifying symbol.
;;; - The synopsis is a descriptive string suitable for short-form help
;;; - The compile field is a procedure that accepts a fol program and
;;;   an optional basename, and compiles that program with this
;;;   backend, leaving the results in a file whose name is derived
;;;   appropriately from that one.  Returns nothing useful and signals
;;;   an error on failure.
;;; - The compiling-problem field is a nullary procedure that examines
;;;   the surrounding system for resources, if any, needed to compile
;;;   with this backend (external executables, etc).  Returns either
;;;   #f (no problem) if the system has all needed resources, or a
;;;   string describing the problem if not.
;;; - The execute field is a procedure that accepts an optional
;;;   basename and executes a fol program previously compiled by the
;;;   compile procedure with that basename.  Any value returned by the
;;;   toplevel form of the fol program is printed to standard output.
;;;   Returns nothing useful and signals an error on failure.
;;; - The executing-problem field is a nullary procedure that examines
;;;   the surrounding system for resources, if any, needed to execute
;;;   programs compiled with this backend (external executables, etc).
;;;   Returns either #f (no problem) if the system has all needed
;;;   resources, or a string describing the problem if not.

(define-structure backend
  name
  synopsis
  compile
  compiling-problem
  execute
  executing-problem)

(define the-backends '())

(define-syntax define-backend
  (syntax-rules ()
    ((_ name arg ...)
     (begin
       (define name (make-backend 'name arg ...))
       (set! the-backends
             (append the-backends (list (cons 'name name))))))))

(define (executable-present? name)
  (let ((location
         (with-output-to-string
           (lambda ()
             (run-shell-command (string-append "which " name))))))
    (> (string-length location) 0)))

(define always-can (lambda () #f))
(define (never-can msg) (lambda () msg))
(define (needs-executable name #!optional msg)
  (lambda ()
    (if (executable-present? name)
        #f
        (string-append "the " name " program is not on the path"
                       (if (default-object? msg) "" msg)))))

(define-backend mit-scheme
  "Compile to native code via MIT Scheme"
  fol->mit-scheme always-can
  ;; run-mit-scheme returns the value without printing it, which
  ;; behavior needs to be preserved until a cleaner library linkage
  ;; mechanism is defined.
  (lambda args (pp (apply run-mit-scheme args))) always-can)

(define-backend floating-mit-scheme
  "Like mit-scheme, but force floating-point arithmetic"
  fol->floating-mit-scheme always-can
  (lambda args (pp (apply run-mit-scheme args))) always-can)

(define-backend standalone-mit-scheme
  "Like mit-scheme, but include FOL runtime"
  fol->standalone-mit-scheme always-can
  run-standalone-mit-scheme (needs-executable "mit-scheme"))

(define-backend stalin
  "Compile to native code via the Stalin Scheme compiler"
  fol->stalin (needs-executable "stalin")
  run-stalin always-can)

(define-backend common-lisp
  "Generate Common Lisp and compile to native code via SBCL"
  fol->common-lisp (needs-executable "sbcl")
  run-common-lisp (needs-executable "sbcl"))

(define-backend haskell
  "Generate Haskell source and compile to native code via GHC"
  fol->haskell (needs-executable "fol2hs" "; did you cabal install from haskell-fol?") ; Also ghc
  run-haskell always-can)
