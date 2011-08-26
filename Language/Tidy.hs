{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Tidy where

import FOL.Language.Common
import FOL.Language.Expression

import Data.List
import Data.Generics.Uniplate.Operations

import Control.Monad

tidyExpr :: Expr -> Expr
tidyExpr = rewrite rule
    where
      rule e = msum (map ($ e) tidyRules)

tidyRules :: [Expr -> Maybe Expr]
tidyRules = [tidyLetValues]

tidyLetValues :: Expr -> Maybe Expr
tidyLetValues e@(LetValues (Bindings bs) body)
    | null bs1
    = Nothing
    | otherwise
    = Just $ mkLet bs1' $ mkLetValues bs2 body
    where
      (bs1, bs2) = partition is_values bs
      bs1' = concat [zip xs es | (xs, Values es) <- bs1]
      is_values (_, Values _) = True
      is_values _             = False
tidyLetValues _ = Nothing

tidyDefn :: Defn -> Defn
tidyDefn (Defn proc args body) = Defn proc args (tidyExpr body)

tidyProg :: Prog -> Prog
tidyProg (Prog defns expr) = Prog (map tidyDefn defns) (tidyExpr expr)

tidy :: Prog -> Prog
tidy = tidyProg
