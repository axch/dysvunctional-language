# -*- mode: ruby -*-

require 'rake/clean'

CLEAN.include("**/*.o", "**/*.hi", "**/*.bci", "**/*.bin", "**/*.com")
CLOBBER.include("fol")

options = "-O2 -Wall"
sources = FileList["Language/*.hs", "Optimize/*.hs"]

task :default => [:test]

file "fol" => ["fol.hs"] + sources do
  sh "ghc #{options} -o fol #{sources} fol.hs"
end

task :test => ["fol"] do sh <<EOF
mit-scheme \
--compiler \
--batch-mode \
--no-init-file \
--eval '(set! load/suppress-loading-message? #t)' \
--eval '(begin (load "scm/load") (show-time run-registered-tests) (newline) (flush-output) (%exit 0))'
EOF
end
