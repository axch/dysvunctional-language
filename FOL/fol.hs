module Main where

import FOL.Optimize.Optimize

main :: IO ()
main = interact optimize >> putChar '\n'