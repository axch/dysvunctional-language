{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.TypeCheck where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Data.List
import Data.Maybe

import Debug.Trace

import Text.Printf

data Type = PrimTy Shape
          | ProcTy [Shape] Shape
            deriving (Eq, Show)

instance Pretty Type where
    pp (PrimTy shape) = pp shape
    pp (ProcTy arg_shapes ret_shape)
        = ppList [text "procedure", ppList (map pp arg_shapes), pp ret_shape]

type TyEnv = [(Name, Type)]

typesOfPrimitives :: TyEnv
typesOfPrimitives = [ (Name "abs",       r2r  )
                    , (Name "exp",       r2r  )
                    , (Name "log",       r2r  )
                    , (Name "sin",       r2r  )
                    , (Name "cos",       r2r  )
                    , (Name "tan",       r2r  )
                    , (Name "asin",      r2r  )
                    , (Name "acos",      r2r  )
                    , (Name "sqrt",      r2r  )
                    , (Name "real",      r2r  )
                    , (Name "+",         rxr2r)
                    , (Name "-",         rxr2r)
                    , (Name "*",         rxr2r)
                    , (Name "/",         rxr2r)
                    , (Name "atan",      rxr2r)
                    , (Name "expt",      rxr2r)
                    , (Name "zero?",     r2b  )
                    , (Name "positive?", r2b  )
                    , (Name "negative?", r2b  )
                    , (Name "<",         rxr2b)
                    , (Name "<=",        rxr2b)
                    , (Name ">",         rxr2b)
                    , (Name ">=",        rxr2b)
                    , (Name "=",         rxr2b)
                    ]
    where
      r2r   = ProcTy [RealSh] RealSh
      rxr2r = ProcTy [RealSh, RealSh] RealSh
      r2b   = ProcTy [RealSh] BoolSh
      rxr2b = ProcTy [RealSh, RealSh] BoolSh

tc :: Prog -> Type
tc = tcProg typesOfPrimitives

tcProg :: TyEnv -> Prog -> Type
tcProg env (Prog defns expr) = tcExpr (tcDefns env defns ++ env) expr

-- The declared type of a given procedure.
declProcType :: Defn -> Type
declProcType (Defn proc args body) = ProcTy arg_shapes proc_shape
    where
      proc_shape = snd proc
      arg_shapes = map snd args

tcDefns :: TyEnv -> [Defn] -> [(Name, Type)]
tcDefns env defns
    = [(procName defn, tcDefn env' defn)
           | defn <- defns
           , let env' = [(procName defn', declProcType defn')
                             | defn' <- defns, defn' /= defn] ++ env]

procName :: Defn -> Name
procName (Defn (proc_name, _) _ _) = proc_name

tcDefn :: TyEnv -> Defn -> Type
tcDefn env defn@(Defn proc args body)
    | proc_type == PrimTy proc_shape
    = declProcType defn
    | otherwise
    = error $ unwords [ "The inferred return type"
                      , pprint proc_type
                      , "of procedure"
                      , pprint proc_name
                      , "doesn't match its declared type"
                      , pprint proc_shape
                      ]
    where
      (proc_name, proc_shape) = proc
      env' = [(arg_name, PrimTy arg_shape)
                  | (arg_name, arg_shape) <- args] ++ env
      proc_type = tcExpr env' body

tcExpr :: TyEnv -> Expr -> Type
tcExpr env (Var x)
    = case lookup x env of
        Just (t@(PrimTy _)) -> t
        Just _  -> error $ "Variable bound to a procedure: " ++ pprint x
        Nothing -> error $ "Unbound variable: " ++ pprint x
tcExpr _ Nil      = PrimTy NilSh
tcExpr _ (Bool _) = PrimTy BoolSh
tcExpr _ (Real _) = PrimTy RealSh
tcExpr env e@(If p c a)
    = case (tcExpr env p) of
        PrimTy BoolSh ->
            let tc = tcExpr env c
                ta = tcExpr env a
            in if tc == ta
               then tc
               else error $ unlines [ "Branches of IF expression"
                                    , pprint e
                                    , unwords [ "have different types:"
                                              , pprint tc
                                              , "and"
                                              , pprint ta
                                              ]
                                    ]
        ty -> error $ unlines [ "The predicate of IF expression"
                              , pprint e
                              , unwords [ "is expected to have type"
                                        , pprint (PrimTy BoolSh)
                                        , "but the inferred type is"
                                        , pprint ty
                                        ]
                              ]
tcExpr env (Let bindings body) = tcExpr env' body
    where
      env' = [(x, tcExpr env e) | (x, e) <- bindings] ++ env
tcExpr env e@(LetValues bindings body) = tcExpr env' body
    where
      env' = concatMap destructure bindings ++ env
      destructure (xs, e) = destructure' xs (tcExpr env e)
          where
            destructure' xs (PrimTy (ValuesSh ss))
                | length xs == length ss
                = zip xs (map PrimTy ss)
                | otherwise
                = error $ unwords [ "The number of variables in the pattern"
                                  , render (ppList (map pp xs))
                                  , "does not match the number of values in"
                                  , pprint e
                                  ]
            destructure' xs _ = error $ "An expression of shape VALUES is expected"
tcExpr env e@(Car e')
    | PrimTy (ConsSh t _) <- t'
    = PrimTy t
    | otherwise
    = error $ unlines [ "The expression"
                      , pprint e'
                      , "is expected to have shape CONS, but the inferred type is"
                      , pprint t'
                      ]
    where
      t' = tcExpr env e'

tcExpr env e@(Cdr e')
    | PrimTy (ConsSh _ t) <- t'
    = PrimTy t
    | otherwise
    = error $ unlines [ "The expression"
                      , pprint e'
                      , "is expected to have shape CONS, but the inferred type is"
                      , pprint t'
                      ]
    where
      t' = tcExpr env e'

tcExpr env e@(VectorRef e' i)
    | PrimTy (VectorSh ss) <- t'
    = PrimTy (ss !! i)
    | otherwise
    = error $ unlines [ "The expression"
                      , pprint e'
                      , "is expected to have shape VECTOR, but the inferred type is"
                      , pprint t'
                      ]
    where
      t' = tcExpr env e'

tcExpr env e@(Cons e1 e2)
    | PrimTy s1 <- tcExpr env e1
    , PrimTy s2 <- tcExpr env e2
    = PrimTy (ConsSh s1 s2)
    | otherwise
    = error $ "The arguments of CONS can't be procedures: " ++ pprint e

tcExpr env e@(Vector es) = PrimTy (VectorSh ss)
    where
      ss = map (fromPrimTy . tcExpr env) es

tcExpr env e@(Values es) = PrimTy (ValuesSh ss)
    where
      ss = map (fromPrimTy . tcExpr env) es

tcExpr env e@(ProcCall proc args)
    | Just (ProcTy arg_shapes proc_shape) <- lookup proc env
    = if (length args == length arg_shapes) &&
              and [tcExpr env arg == PrimTy arg_shape
                       | (arg, arg_shape) <- zip args arg_shapes]
      then PrimTy proc_shape
      else error $ "Ill-typed procedure call: " ++ pprint e
    | otherwise
    = error $ "Undefined procedure: " ++ pprint proc

fromPrimTy :: Type -> Shape
fromPrimTy (PrimTy s) = s
fromPrimTy t          = error $ unwords [ "Procedure type"
                                        , pprint t
                                        , "where a shape is expected"
                                        ]
