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

liftPair :: Applicative f => (f a, f b) -> f (a, b)
liftPair = uncurry (liftA2 (,))

-- (<***>) and (<&&&>) are like (***) and (&&&) for Kleisli arrows of
-- a monad, but for an arbitrary applicative functor.
(<***>) :: Applicative f => (b -> f b') -> (c -> f c') -> ((b, c) -> f (b', c'))
g <***> h = liftPair . (g *** h)
infixr 3 <***>

(<&&&>) :: Applicative f => (b -> f c) -> (b -> f c') -> (b -> f (c, c'))
g <&&&> h = liftPair . (g &&& h)
infixr 3 <&&&>

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
tcDefns env defns
    = sequence
      [(return . procName <&&&> tcDefn env') defn
           | defn <- defns
           , let env' = [(procName defn', declProcType defn')
                             | defn' <- defns, defn' /= defn] ++ env]

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
        Just _  -> fail $ "Variable bound to a procedure: " ++ pprint x
        Nothing -> fail $ "Unbound variable: " ++ pprint x
tcExpr _ Nil      = return $ PrimTy NilSh
tcExpr _ (Bool _) = return $ PrimTy BoolSh
tcExpr _ (Real _) = return $ PrimTy RealSh
tcExpr env e@(If p c a)
    = do tp <- tcExpr env p
         case tp of
           PrimTy BoolSh ->
               do tc <- tcExpr env c
                  ta <- tcExpr env a
                  if tc == ta
                    then return tc
                    else fail $ unlines [ "Branches of IF expression"
                                        , pprint e
                                        , unwords [ "have different types:"
                                                  , pprint tc
                                                  , "and"
                                                  , pprint ta
                                                  ]
                                        ]
           ty -> fail $ unlines [ "The predicate of IF expression"
                                , pprint e
                                , unwords [ "is expected to have type"
                                          , pprint (PrimTy BoolSh)
                                          , "but the inferred type is"
                                          , pprint ty
                                          ]
                                ]
tcExpr env (Let bindings body)
    = do env' <- mapM (return <***> tcExpr env) bindings
         tcExpr (env' ++ env) body
tcExpr env e@(LetValues bindings body)
    = do env' <- concatMapM destructure bindings
         tcExpr (env' ++ env) body
    where
      destructure (xs, e) = destructure' xs =<< tcExpr env e
          where
            destructure' xs (PrimTy (ValuesSh ss))
                | length xs == length ss
                = return $ zip xs (map PrimTy ss)
                | otherwise
                = fail $ unwords [ "The number of variables in the pattern"
                                 , render (ppList (map pp xs))
                                 , "does not match the number of values in"
                                 , pprint e
                                 ]
            destructure' xs _ = fail "An expression of shape VALUES is expected"
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

tcExpr env e@(Cons e1 e2)
    = do t <- liftA2 (,) (tcExpr env e1) (tcExpr env e2)
         case t of
           (PrimTy s1, PrimTy s2) -> return $ PrimTy (ConsSh s1 s2)
           _ -> fail $ "The arguments of CONS can't be procedures: " ++ pprint e

tcExpr env e@(Vector es) = liftM (PrimTy . VectorSh) ss
    where
      ss = mapM (fromPrimTy <=< tcExpr env) es

tcExpr env e@(Values es) = liftM (PrimTy . ValuesSh) ss
    where
      ss = mapM (fromPrimTy <=< tcExpr env) es

tcExpr env e@(ProcCall proc args)
    | Just (ProcTy arg_shapes proc_shape) <- lookup proc env
    , let nargs = length args
    , let narg_shapes = length arg_shapes
    = if nargs == narg_shapes
      then mapM_ testArgType (zip args arg_shapes) >>= (const . return $ PrimTy proc_shape)
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
fromPrimTy t          = fail $ unwords [ "Procedure type"
                                       , pprint t
                                       , "where a shape is expected"
                                       ]
