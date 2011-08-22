{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Optimize.Inline where

import FOL.Language.AlphaRn
import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Feedback
import FOL.Language.Unique

import Data.Generics.Uniplate.Data

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

inline :: Prog -> Unique Prog
inline = alphaRn . inlineProg

-- Compute the list of procedures called within a given expression.
callees :: Expr -> [Name]
callees e = nub [proc | ProcCall proc _ <- universe e]

inlineProg :: Prog -> Prog
inlineProg (Prog defns expr) = Prog defns' expr'
    where
      non_inlinable = feedbackVertexSet call_graph
      call_graph    = [(proc_name, callees body \\ primitives)
                           | Defn (proc_name, _) _ body <- defns]
      -- A map mapping the names of the procedures that are
      -- to be inlined to their definitions.
      defn_map      = Map.fromList
                      [(proc_name, defn)
                           | defn@(Defn (proc_name, _) _ _) <- defns
                           , proc_name `notElem` non_inlinable]
      -- Leave only the procedure definitions that have not
      -- been inlined, inlining their bodies.
      defns'        = [inlineDefn defn_map defn
                           | defn@(Defn (proc_name, _) _ _) <- defns
                           , proc_name `elem` non_inlinable]
      expr'         = inlineExpr defn_map expr

-- Inline given procedure definitions in a given definition.
inlineDefn :: Map Name Defn -> Defn -> Defn
inlineDefn defn_map (Defn proc args body)
    = Defn proc args (inlineExpr defn_map body)

-- Inline given procedure definitions in a given expression.
inlineExpr :: Map Name Defn -> Expr -> Expr
inlineExpr defn_map = transform (maybeInline defn_map)

maybeInline :: Map Name Defn -> Expr -> Expr
maybeInline defn_map e@(ProcCall name args)
    | Just (Defn _ vars body) <- Map.lookup name defn_map
    , let bindings = zip (map fst vars) args
    -- 'body' may still be inlinable
    = Let bindings (inlineExpr defn_map body)
    | otherwise
    = e
maybeInline _ e = e
