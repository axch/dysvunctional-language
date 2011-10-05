(declare (usual-integrations))

(declare (integrate-external "../pattern-matching"))

(in-test-group
 pattern-matching

 (define-test (test-smoke)
   (check (= 0 0))))
