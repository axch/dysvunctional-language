{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.TypeCheck where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Control.Applicative
import Control.Monad

import Data.List
import Data.Maybe

import Debug.Trace

-- Types of entities in FOL programs.  In addition to shapes that
-- values may have there are also procedure types.
data Type = PrimTy Shape
          | ProcTy [Shape] Shape
            deriving (Eq, Show)

instance Pretty Type where
    pp (PrimTy shape) = pp shape
    pp (ProcTy arg_shapes ret_shape)
        = ppList [symbol "procedure", ppList (map pp arg_shapes), pp ret_shape]

-- Possible type checker errors.
data TCError
    -- The inferred return type of a procedure does not match its
    -- declared return type.
    = ProcReturnTypeMismatch
        Name           -- procedure name
        Type           -- its inferred type
        Shape          -- its declared shape
    -- Procedure name that is used in non-operator position.
    | ProcNameUsedAsVariable
        Name           -- procedure name
    -- Branches of an IF expression have different types.
    | IfBranchesTypeMismatch
        Expr           -- IF expression
        Type           -- type of consequent
        Type           -- type of alternate
    -- The number of variables in a LET-VALUES binding pattern is
    -- different from the number of values returned from the
    -- expression.
    | PatternVarsNumMismatch
        [Name]         -- pattern
        Expr           -- expression
    -- Procedure is applied to the wrong (different from declared
    -- in the definition) number of arguments.
    | ProcArgsNumMismatch
        Name           -- procedure name
        Type           -- procedure type
        Int            -- number of arguments given
    -- Procedure argument has the wrong (different from declared)
    -- shape.
    | ProcArgTypeMismatch
        Expr           -- procedure call
        Expr           -- argument
        Type           -- argument type
        Shape          -- expected argument shape
    -- Expected shape differs from the inferred type.
    | ShapeMismatch
        Expr           -- expression
        Type           -- its type
        String         -- shape it is expected to have
    | UnboundVariable
        Name
    | UndefinedProc
        Name
      deriving (Eq, Show)

instance Pretty TCError where
    pp (ProcReturnTypeMismatch proc_name ret_type ret_shape)
        = fsep [ text "The inferred return type"
               , nest 2 (pp ret_type)
               , text "of the procedure"
               , pp proc_name
               , text "doesn't match its declared return type"
               , nest 2 (pp ret_shape)
               ]
    pp (ProcNameUsedAsVariable proc_name)
        = fsep [ text "Procedure name"
               , pp proc_name
               , text "is used as a variable"
               ]
    pp (IfBranchesTypeMismatch expr type1 type2)
        = fsep [ text "The branches of the expression"
               , nest 2 (pp expr)
               , text "have different types"
               , nest 2 (pp type1)
               , text "and"
               , nest 2 (pp type2)
               ]
    pp (PatternVarsNumMismatch names expr)
        = fsep [ text "The number of pattern variables in the pattern"
               , nest 2 (ppList (map pp names))
               , text "doesn't match the number of values returned from"
               , nest 2 (pp expr)
               ]
    pp (ProcArgsNumMismatch proc_name proc_type@(ProcTy arg_shapes _) n)
        = fsep [ text "The procedure"
               , pp proc_name
               , text "is applied to"
               , int n
               , text "arguments,"
               , text "but its type"
               , pp proc_type
               , text "has"
               , int (length arg_shapes)
               ]
    pp (ProcArgTypeMismatch proc_call arg arg_type arg_shape)
        = sep [ text "The argument"
              , nest 2 (pp arg)
              , text "in the procedure call"
              , nest 2 (pp proc_call)
              , fsep [ text "is expected to have shape"
                     , pp arg_shape <> comma
                     , text "but its inferred type is"
                     , pp arg_type
                     ]
              ]
    pp (ShapeMismatch expr expr_type shape)
        = fsep [ text "The expression"
               , nest 2 (pp expr)
               , text "is expected to have shape"
               , symbol shape <> comma
               , text "but its inferred type is"
               , pp expr_type
               ]
    pp (UnboundVariable name)
        = fsep [ text "Unbound variable:"
               , pp name
               ]
    pp (UndefinedProc name)
        = fsep [ text "Undefined procedure:"
               , pp name
               ]

-- Type checker monad.
data TC a = TCError TCError | TCOk a deriving (Eq, Show)

instance Monad TC where
    return = TCOk
    TCError msg >>= _ = TCError msg
    TCOk res >>= f = f res

instance Functor TC where
    fmap = liftM

instance Applicative TC where
    pure = return
    (<*>) = ap

tcFail :: TCError -> TC a
tcFail = TCError

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
declProcType (Defn proc args body) = ProcTy arg_shapes ret_shape
    where
      ret_shape  = snd proc
      arg_shapes = map snd args

tcDefns :: TyEnv -> [Defn] -> TC TyEnv
tcDefns env defns = mapM tcDefn' defns
    where
      tcDefn' defn = do proc_type <- tcDefn (env' ++ env) defn
                        return (procName defn, proc_type)
      env' = [(procName defn, declProcType defn) | defn <- defns]

tcDefn :: TyEnv -> Defn -> TC Type
tcDefn env defn@(Defn proc args body)
    = do ret_type <- tcExpr (env' ++ env) body
         if ret_type == PrimTy ret_shape
            then return $ declProcType defn
            else tcFail $ ProcReturnTypeMismatch proc_name ret_type ret_shape
    where
      (proc_name, ret_shape) = proc
      env' = [(arg_name, PrimTy arg_shape) | (arg_name, arg_shape) <- args]

tcExpr :: TyEnv -> Expr -> TC Type
tcExpr env (Var x)
    = case lookup x env of
        Just (t@(PrimTy _)) -> return t
        Just _              -> tcFail $ ProcNameUsedAsVariable x
        Nothing             -> tcFail $ UnboundVariable x
tcExpr _ Nil      = return $ PrimTy NilSh
tcExpr _ (Bool _) = return $ PrimTy BoolSh
tcExpr _ (Real _) = return $ PrimTy RealSh
tcExpr env e@(If p c a) = tcPredicate >> tcBranches
    where
      tcPredicate
          = do tp <- tcExpr env p
               unless (tp == PrimTy BoolSh) $
                 tcFail $ ShapeMismatch p tp "bool"
      tcBranches
          = do tc <- tcExpr env c
               ta <- tcExpr env a
               if tc == ta
                  then return tc
                  else tcFail $ IfBranchesTypeMismatch e tc ta

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
                = tcFail $ PatternVarsNumMismatch xs e
            destructure xs t
                = tcFail $ ShapeMismatch e t "values"

tcExpr env e@(Car e')
    = do t' <- tcExpr env e'
         case t' of
           PrimTy (ConsSh t _) -> return $ PrimTy t
           _ -> tcFail $ ShapeMismatch e' t' "cons"

tcExpr env e@(Cdr e')
    = do t' <- tcExpr env e'
         case t' of
           PrimTy (ConsSh _ t) -> return $ PrimTy t
           _ -> tcFail $ ShapeMismatch e' t' "cons"

tcExpr env e@(VectorRef e' i)
    = do t' <- tcExpr env e'
         case t' of
           PrimTy (VectorSh ss) -> return $ PrimTy (ss !! i)
           _ -> tcFail $ ShapeMismatch e' t' "vector"

tcExpr env (Cons e1 e2) = liftM PrimTy $ liftA2 ConsSh s1 s2
    where
      s1 = exprShape env e1
      s2 = exprShape env e2

tcExpr env (Vector es) = liftM (PrimTy . VectorSh) ss
    where
      ss = mapM (exprShape env) es

tcExpr env (Values es) = liftM (PrimTy . ValuesSh) ss
    where
      ss = mapM (exprShape env) es

tcExpr env e@(ProcCall proc args)
    | Just proc_type@(ProcTy arg_shapes ret_shape) <- lookup proc env
    , let nargs = length args
    , let narg_shapes = length arg_shapes
    = if nargs == narg_shapes
      then mapM_ checkArgType (zip args arg_shapes)
               >> (return $ PrimTy ret_shape)
      else tcFail $ ProcArgsNumMismatch proc proc_type nargs
    | otherwise
    = tcFail $ UndefinedProc proc
    where
      checkArgType (arg, arg_shape)
          = do arg_type <- tcExpr env arg
               if arg_type == PrimTy arg_shape
                  then return arg_type
                  else tcFail $ ProcArgTypeMismatch e arg arg_type arg_shape

fromPrimTy :: Type -> TC Shape
fromPrimTy (PrimTy s) = return s
-- 'fromPrimTy' is only used where the following cannot happen.  We
-- add a catch-all case anyway.
fromPrimTy _          = fail "Procedure type where a shape is expected"

exprShape :: TyEnv -> Expr -> TC Shape
exprShape env = fromPrimTy <=< tcExpr env
