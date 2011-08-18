{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.TypeCheck where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Control.Applicative
import Control.Arrow
import Control.Monad

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

data TC a = TCError String | TCOk a deriving (Eq, Show)

instance Monad TC where
    return = TCOk
    TCError msg >>= _ = TCError msg
    TCOk res >>= f = f res
    fail msg = TCError msg

instance Functor TC where
    fmap = liftM

instance Applicative TC where
    pure = return
    (<*>) = ap

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

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

tc :: Prog -> TC Type
tc = tcProg typesOfPrimitives

tcProg :: TyEnv -> Prog -> TC Type
tcProg env (Prog defns expr)
    = do env' <- tcDefns env defns
         tcExpr (env' ++ env) expr

-- The declared type of a given procedure.
declProcType :: Defn -> Type
declProcType (Defn proc args body) = ProcTy arg_shapes proc_shape
    where
      proc_shape = snd proc
      arg_shapes = map snd args

tcDefns :: TyEnv -> [Defn] -> TC TyEnv
tcDefns env defns = mapM tcDefn' defns
    where
      tcDefn' defn = do proc_type <- tcDefn (env' ++ env) defn
                        return (procName defn, proc_type)
          where
            env' = [(procName defn', declProcType defn')
                        | defn' <- defns, defn' /= defn]

procName :: Defn -> Name
procName (Defn (proc_name, _) _ _) = proc_name

tcDefn :: TyEnv -> Defn -> TC Type
tcDefn env defn@(Defn proc args body)
    = do proc_type <- tcExpr env' body
         if proc_type == PrimTy proc_shape
            then return $ declProcType defn
            else fail $ unwords [ "The inferred return type"
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

tcExpr :: TyEnv -> Expr -> TC Type
tcExpr env (Var x)
    = case lookup x env of
        Just (t@(PrimTy _)) -> return t
        -- The only way for a name to be bound to a procedure type is
        -- if that name denotes a global procedure (i.e., a primitive
        -- or a procedure defined using DEFINE).  Hence the following
        -- error message.
        Just _  -> fail $ "Procedure name used as a variable: " ++ pprint x
        Nothing -> fail $ "Unbound variable: " ++ pprint x
tcExpr _ Nil      = return $ PrimTy NilSh
tcExpr _ (Bool _) = return $ PrimTy BoolSh
tcExpr _ (Real _) = return $ PrimTy RealSh
tcExpr env e@(If p c a) = tcPredicate >> tcBranches
    where
      tcPredicate
          = do tp <- tcExpr env p
               case tp of
                 PrimTy BoolSh -> return ()
                 _ -> fail $ unwords [ "The pridicate of IF expression"
                                     , pprint e
                                     , "is expected to have type"
                                     , pprint (PrimTy BoolSh)
                                     , "but the inferred type is"
                                     , pprint tp
                                     ]
      tcBranches
          = do tc <- tcExpr env c
               ta <- tcExpr env a
               if tc == ta
                  then return tc
                  else fail $ unwords [ "The branches of IF expression"
                                      , pprint e
                                      , "have different types"
                                      , pprint tc
                                      , "and"
                                      , pprint ta
                                      ]

tcExpr env (Let bindings body)
    = do env' <- mapM tcBinding bindings
         tcExpr (env' ++ env) body
    where
      tcBinding (x, e) = do t <- tcExpr env e
                            return (x, t)
tcExpr env e@(LetValues bindings body)
    = do env' <- concatMapM tcBinding bindings
         tcExpr (env' ++ env) body
    where
      tcBinding (xs, e) = destructure xs =<< tcExpr env e
          where
            destructure xs (PrimTy (ValuesSh ss))
                | length xs == length ss
                = return $ zip xs (map PrimTy ss)
                | otherwise
                = fail $ unwords [ "The number of variables in the pattern"
                                 , render (ppList (map pp xs))
                                 , "does not match the number of values in"
                                 , pprint e
                                 ]
            destructure xs _ = fail "An expression of shape VALUES is expected"
tcExpr env e@(Car e')
    = do t' <- tcExpr env e'
         case t' of
           PrimTy (ConsSh t _) -> return $ PrimTy t
           _ -> fail $ unlines [ "The expression"
                               , pprint e'
                               , "is expected to have shape CONS, but the inferred type is"
                               , pprint t'
                               ]

tcExpr env e@(Cdr e')
    = do t' <- tcExpr env e'
         case t' of
           PrimTy (ConsSh _ t) -> return $ PrimTy t
           _ -> fail $ unlines [ "The expression"
                               , pprint e'
                               , "is expected to have shape CONS, but the inferred type is"
                               , pprint t'
                               ]

tcExpr env e@(VectorRef e' i)
    = do t' <- tcExpr env e'
         case t' of
           PrimTy (VectorSh ss) -> return $ PrimTy (ss !! i)
           _ -> fail $ unlines [ "The expression"
                               , pprint e'
                               , "is expected to have shape VECTOR, but the inferred type is"
                               , pprint t'
                               ]

tcExpr env (Cons e1 e2) = liftM PrimTy $ liftA2 ConsSh tcCar tcCdr
    where
      tcCar = fromPrimTy =<< tcExpr env e1
      tcCdr = fromPrimTy =<< tcExpr env e2

tcExpr env (Vector es) = liftM (PrimTy . VectorSh) ss
    where
      ss = mapM (fromPrimTy <=< tcExpr env) es

tcExpr env (Values es) = liftM (PrimTy . ValuesSh) ss
    where
      ss = mapM (fromPrimTy <=< tcExpr env) es

tcExpr env e@(ProcCall proc args)
    | Just (ProcTy arg_shapes proc_shape) <- lookup proc env
    , let nargs = length args
    , let narg_shapes = length arg_shapes
    = if nargs == narg_shapes
      then mapM_ testArgType (zip args arg_shapes) >> (return $ PrimTy proc_shape)
      else fail $ unwords [ "Procedure"
                          , pprint proc
                          , "needs"
                          , show narg_shapes
                          , "arguments, but is given"
                          , show nargs
                          ]
    | otherwise
    = error $ "Undefined procedure: " ++ pprint proc
    where
      testArgType (arg, arg_shape)
          = do targ <- tcExpr env arg
               if targ == PrimTy arg_shape
                  then return targ
                  else fail $ unwords [ "The argument"
                                      , pprint arg
                                      , "of the procedure call"
                                      , pprint e
                                      , "is expected to have type"
                                      , pprint (PrimTy arg_shape)
                                      , "but the inferred type is"
                                      , pprint targ
                                      ]

fromPrimTy :: Type -> TC Shape
fromPrimTy (PrimTy s) = return s
-- 'fromPrimTy' should be used only where the following cannot happen.
--  We add a catch-all case anyway.
fromPrimTy _          = fail "Procedure type where a shape is expected"
