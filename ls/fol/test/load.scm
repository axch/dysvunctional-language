(load-relative "../../testing/load" fol-environment)

(for-each
 (lambda (file)
   (load-relative-compiled file fol-environment))
 '("fol-test"
   "cse-test"
   "interactions-test"
   "backend-test"))
