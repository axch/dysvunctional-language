(load-relative "../../testing/load")

(for-each
 (lambda (file)
   (load-relative-compiled file))
 '("pattern-matching-test"))
