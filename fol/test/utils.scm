;;; ----------------------------------------------------------------------
;;; Copyright 2010-2011 National University of Ireland.
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

;;; Two more adverbs that are useful in tests

(define (carefully stage-data)
  (define (check-annotations program)
    (if (present? 'structures-as-vectors program)
        (check (equal? program (structure-definitions->vectors program))))
    (if (present? 'unique-names program)
        (check (unique-names? program)))
    (if (present? 'type program)
        (let ((annotated-type (property-value 'type program)))
          (check (equal-type?
                  annotated-type
                  (check-program-types program)
                  ;; TODO Actually, I should use the type map from the
                  ;; pre-transform program, because the transform may
                  ;; remove but may not add type names.
                  (type-map program)))))
    (if (present? 'a-normal-form program)
        (check (approximate-anf? program)))
    (if (present? 'lets-lifted program)
        (check (lets-lifted? program)))
    (if (present? 'fully-inlined program)
        (check (equal? program (inline program))))
    (if (and (present? 'aggregates-replaced program)
             (present? 'a-normal-form program))
        (check (equal? program (scalar-replace-aggregates program))))
    (if (present? 'dead-types-eliminated program)
        (check (equal? program (eliminate-dead-types program))))
    (if (and (present? 'no-common-subexpressions program)
             (present? 'a-normal-form program)
             (present? 'lets-lifted program))
        (check (equal? program (intraprocedural-cse program))))
    (if (present? 'no-intraprocedural-dead-variables program)
        (check (equal? program
                       (eliminate-intraprocedural-dead-code program))))
    (if (present? 'no-interprocedural-dead-variables program)
        (check (equal? program
                       (eliminate-interprocedural-dead-code program))))
    program)
  (lambda (exec)
    (lambda (program . extra)
      ;; TODO Also check counterfactual invariants:
      ;; - Inlining commutes with ANF up to removal of aliases.  Why
      ;;   aliases?  Because inlining saves ANF work by naming the
      ;;   expressions that are arguments to inlined procedures.
      ;; - Inlining preserves ANF
      ;; - Inlining commutes with SRA+ANF up to aliases.
      (check-annotations (apply exec program extra)))))

(define ((meticulously answer) stage-data)
  (lambda (exec)
    (lambda (program)
      (abegin1
       (((carefully stage-data) exec) program)
       (check (equal? answer (fol-eval it)))))))
