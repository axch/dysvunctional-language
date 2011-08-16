{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.TypeCheck where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Data.Maybe

data Type = PrimTy Shape
          | ProcTy [Shape] Shape
            deriving (Eq, Show)

instance Pretty Type where
    pp (PrimTy shape) = pp shape
    pp (ProcTy arg_shapes res_shape)
        = ppList [ text "procedure"
                 , ppList (map pp arg_shapes)
                 , pp res_shape
                 ]

type TyEnv = [(Name, Type)]

tcExpr :: TyEnv -> Expr -> Type
tcExpr env (Var x) = fromMaybe msg (lookup x env)
    where
      msg = error $ "Unbound variable: " ++ show x
tcExpr _ Nil = PrimTy NilSh
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
                                              , show tc
                                              , "and"
                                              , show ta
                                              ]
                                    ]
        ty -> error $ unlines [ "The predicate of IF expression"
                              , pprint e
                              , unwords [ "is expected to have type"
                                        , show (PrimTy BoolSh)
                                        , "but the inferred type is"
                                        , show ty
                                        ]
                              ]
tcExpr env (Let bindings body) = tcExpr (zip xs ts ++ env) body
    where
      (xs, es) = unzip bindings
      ts = map (tcExpr env) es
tcExpr env e@(LetValues bindings body) = tcExpr env' body
    where
      env' = concatMap destructure bindings ++ env
      destructure (xs, e) = destructure' xs ss
          where
            ss = tcExpr env e
            destructure' xs (PrimTy (ValuesSh ss))
                | length xs == length ss
                = zip xs (map PrimTy ss)
                | otherwise
                = error $ unlines [ "The number of variables in the pattern"
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
      fromPrimTy (PrimTy s) = s
      fromPrimTy t          = error $ "Non-shape " ++ pprint t ++ "where a shape is expected"

tcExpr env e@(Values es) = PrimTy (ValuesSh ss)
    where
      ss = map (fromPrimTy . tcExpr env) es
      fromPrimTy (PrimTy s) = s
      fromPrimTy t          = error $ "Non-shape " ++ pprint t ++ "where a shape is expected"

tcExpr env e@(ProcCall proc args)
    | Just (ProcTy arg_shapes proc_shape) <- lookup proc env
    = if (length args == length arg_shapes) &&
              and [tcExpr env arg == PrimTy arg_shape
                       | (arg, arg_shape) <- zip args arg_shapes]
      then (PrimTy proc_shape)
      else error $ "Ill-typed procedure call: " ++ pprint e
    | otherwise
    = error $ "Undefined procedure: " ++ pprint proc
