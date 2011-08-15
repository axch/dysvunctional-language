{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Optimize.Inline where

import FOL.Language.AlphaRn
import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Feedback
import FOL.Language.Unique

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

inline :: Prog -> Unique Prog
inline = alphaRn . inlineProg

nonInlinableDefns :: [Defn] -> [Name]
nonInlinableDefns = feedbackVertexSet . mkCallGraph

mkCallGraph :: [Defn] -> [(Name, [Name])]
mkCallGraph defns
    = [ (proc_name, callees body `intersect` defined_procs)
            | Defn (proc_name, _) _ body <- defns ]
    where
      defined_procs = [proc_name | Defn (proc_name, _) _ _ <- defns]

callees :: Expr -> [Name]
callees (Var x)  = []
callees Nil      = []
callees (Bool _) = []
callees (Real _) = []
callees (If p c a) = concat [ callees p , callees c, callees a]
callees (Let bindings body)
    = concat $ (callees body) : [callees e | (_, e) <- bindings]
callees (LetValues bindings body)
    = concat $ (callees body) : [callees e | (_, e) <- bindings]
callees (Car e) = callees e
callees (Cdr e) = callees e
callees (VectorRef e _) = callees e
callees (Cons e1 e2) = callees e1 ++ callees e2
callees (Vector es) = concatMap callees es
callees (Values es) = concatMap callees es
callees (ProcCall proc args) = proc : concatMap callees args

inlineProg :: Prog -> Prog
inlineProg prog@(Prog defns expr) = Prog defns' expr'
    where
      defined_procs = [proc_name | Defn (proc_name, _) _ _ <- defns]
      non_inlinable = nonInlinableDefns defns
      inlinable     = defined_procs \\ non_inlinable
      inlinable_index
          = Map.fromList [ (proc_name, defn)
                               | defn@(Defn (proc_name, _) _ _) <- defns
                               , proc_name `elem` inlinable ]
      defns' = [ inlineDefn inlinable_index defn
                     | defn@(Defn (proc_name, _) _ _) <- defns
                     , proc_name `elem` non_inlinable ]
      expr'  = inlineExpr inlinable_index expr

inlineDefn :: Map Name Defn -> Defn -> Defn
inlineDefn defns (Defn proc args body)
    = Defn proc args (inlineExpr defns body)

inlineExpr :: Map Name Defn -> Expr -> Expr
inlineExpr _ e@(Var x)  = e
inlineExpr _ e@Nil      = e
inlineExpr _ e@(Bool _) = e
inlineExpr _ e@(Real _) = e
inlineExpr defns (If p c a) = If (inlineExpr defns p) (inlineExpr defns c) (inlineExpr defns a)
inlineExpr defns (Let bindings body)
    = Let bindings' body'
    where
      bindings' = [ (x, e') | (x, e) <- bindings, let e' = inlineExpr defns e ]
      body' = inlineExpr defns body
inlineExpr defns (LetValues bindings body)
    = LetValues bindings' body'
    where
      bindings' = [ (xs, e') | (xs, e) <- bindings, let e' = inlineExpr defns e ]
      body' = inlineExpr defns body
inlineExpr defns (Car e)         = Car (inlineExpr defns e)
inlineExpr defns (Cdr e)         = Cdr (inlineExpr defns e)
inlineExpr defns (VectorRef e i) = VectorRef (inlineExpr defns e) i
inlineExpr defns (Cons e1 e2)    = Cons (inlineExpr defns e1) (inlineExpr defns e2)
inlineExpr defns (Vector es)     = Vector (map (inlineExpr defns) es)
inlineExpr defns (Values es)     = Values (map (inlineExpr defns) es)
inlineExpr defns (ProcCall name args)
    | Just (Defn proc formals body) <- Map.lookup name defns
    , let bindings = zip (map fst formals) args
    = inlineExpr defns $ Let bindings body
    | otherwise
    = ProcCall name (map (inlineExpr defns) args)
