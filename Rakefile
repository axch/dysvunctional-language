# -*- mode: ruby -*-

require 'rake/clean'

CLEAN.include("**/*.o")
CLEAN.include( "**/*.hi")
CLOBBER.include("fol")

FLAGS = "-O2 -Wall"
SRC = FileList["Language/*.hs", "Optimize/*.hs"]

task :default => ["fol"]

file "fol" => ["fol.hs"] + SRC do
  sh "ghc #{FLAGS} -o fol #{SRC} fol.hs"
end
