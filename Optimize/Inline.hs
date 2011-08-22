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
-- The list may contain duplicates, which is fine because we use it
-- only for set manipulations.
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
inlineExpr _ e@(Var _)  = e
inlineExpr _ e@Nil      = e
inlineExpr _ e@(Bool _) = e
inlineExpr _ e@(Real _) = e
inlineExpr defn_map (If p c a)
    = If (inlineExpr defn_map p)
         (inlineExpr defn_map c)
         (inlineExpr defn_map a)
inlineExpr defn_map (Let bindings body)
    = Let bindings' body'
    where
      bindings' = [(x,  e') | (x,  e) <- bindings
                            , let e' = inlineExpr defn_map e]
      body' = inlineExpr defn_map body
inlineExpr defn_map (LetValues bindings body)
    = LetValues bindings' body'
    where
      bindings' = [(xs, e') | (xs, e) <- bindings
                            , let e' = inlineExpr defn_map e]
      body' = inlineExpr defn_map body
inlineExpr defn_map (Car e)         = Car (inlineExpr defn_map e)
inlineExpr defn_map (Cdr e)         = Cdr (inlineExpr defn_map e)
inlineExpr defn_map (VectorRef e i) = VectorRef (inlineExpr defn_map e) i
inlineExpr defn_map (Cons e1 e2)
    = Cons (inlineExpr defn_map e1) (inlineExpr defn_map e2)
inlineExpr defn_map (Vector es)     = Vector (map (inlineExpr defn_map) es)
inlineExpr defn_map (Values es)     = Values (map (inlineExpr defn_map) es)
inlineExpr defn_map (ProcCall name args)
    | Just (Defn _ vars body) <- Map.lookup name defn_map
    , let bindings = zip (map fst vars) args
    = inlineExpr defn_map $ Let bindings body
    | otherwise
    = ProcCall name (map (inlineExpr defn_map) args)
