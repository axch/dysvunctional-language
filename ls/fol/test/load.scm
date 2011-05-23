(for-each
 (lambda (file)
   (load-relative file fol-environment))
 '("../../testing/load"
   "fol-test"))
