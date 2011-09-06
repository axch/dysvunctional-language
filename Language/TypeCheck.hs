{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.TypeCheck where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Control.Applicative
import Control.Monad

import qualified Data.Traversable as Traversable

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
    | ValuesBoundToVariable
        Expr           -- enclosing LET expression
        Name           -- variable name
        Expr           -- offending expression
    | ValuesUsedAsProcArg
        Expr           -- procedure call
        Expr           -- offending argument
    | NestedValues
        Expr           -- enclosing VALUES expression
        Expr           -- offending expression
    | ValuesAsConstructorArg
        Expr           -- enclosing construction
        Expr           -- offending expression
    | ValuesAsDeclProcArg
        Defn           -- definition
        Name           -- offending argument name
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
    pp (ValuesBoundToVariable let_expr name rhs_expr)
        = fsep [ text "Cannot bind expression"
               , nest 2 (pp rhs_expr)
               , hsep [ text "of shape VALUES to variable"
                      , pp name
                      , text "in expression"
                      ]
               , nest 2 (pp let_expr)
               ]
    pp (ValuesUsedAsProcArg proc_call arg)
        = fsep [ text "Expression"
               , nest 2 (pp arg)
               , text "of shape VALUES is used as an argument in procedure call"
               , nest 2 (pp proc_call)
               ]
    pp (NestedValues values_expr expr)
        = fsep [ text "Expression"
               , nest 2 (pp expr)
               , text "of shape VALUES is nested inside VALUES expression"
               , nest 2 (pp values_expr)
               ]
    pp (ValuesAsConstructorArg constr_expr expr)
        = fsep [ text "Expression"
               , nest 2 (pp expr)
               , text "of shape VALUES is used as an argument in construction"
               , nest 2 (pp constr_expr)
               ]
    pp (ValuesAsDeclProcArg defn arg_name)
        = fsep [ hsep [ text "Argument"
                      , pp arg_name
                      , text "in procedure definition"
                      ]
               , nest 2 (pp defn)
               , text "has declared shape VALUES"
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

-- Type check the program and return its type (i.e., the type of the
-- expression given the declared types of the local procedures,
-- provided they are consistent).  Error out on type errors.
tc :: Prog -> Type
tc = fst . ann

-- Given a program, annotate each node with its type, and return the
-- annotated program.  Error out on type errors.
ann :: Prog -> AnnProg Type
ann prog = case annProg typesOfPrimitives prog of
             TCOk a      -> a
             TCError err -> error ("\n" ++ pprint err)

annProg :: TyEnv -> Prog -> TC (AnnProg Type)
annProg init_env (Prog defns expr)
    = do ann_defns               <- mapM (annDefn prog_env) defns
         ann_expr@(prog_type, _) <- annExpr prog_env expr
         return (prog_type, AnnProg ann_defns ann_expr)
    where
      prog_env = [(procName defn, declProcType defn) | defn <- defns] ++ init_env

annDefn :: TyEnv -> Defn -> TC (AnnDefn Type)
annDefn env defn@(Defn proc args body)
    = do mapM_ check_arg_shape args
         ann_body@(ret_type, _) <- annExpr (env' ++ env) body
         if ret_type == PrimTy ret_shape
            then return (declProcType defn, AnnDefn proc args ann_body)
            else tcFail $ ProcReturnTypeMismatch proc_name ret_type ret_shape
    where
      (proc_name, ret_shape) = proc
      env' = [(arg_name, PrimTy arg_shape) | (arg_name, arg_shape) <- args]
      check_arg_shape (arg_name, ValuesSh _)
          = tcFail $ ValuesAsDeclProcArg defn arg_name
      check_arg_shape _
          = return ()

annExpr :: TyEnv -> Expr -> TC (AnnExpr Type)
annExpr env (Var x)
    = case lookup x env of
        Just (t@(PrimTy _)) -> return (t, AnnVar x)
        Just _              -> tcFail $ ProcNameUsedAsVariable x
        Nothing             -> tcFail $ UnboundVariable x
annExpr _ Nil      = return (PrimTy NilSh,  AnnNil   )
annExpr _ (Bool b) = return (PrimTy BoolSh, AnnBool b)
annExpr _ (Real r) = return (PrimTy RealSh, AnnReal r)
annExpr env e@(If p c a)
    = do ann_p@(tp, _) <- annExpr env p
         unless (tp == PrimTy BoolSh) $
            tcFail $ ShapeMismatch p tp "bool"
         ann_c@(tc, _) <- annExpr env c
         ann_a@(ta, _) <- annExpr env a
         if tc == ta
            then return (tc, AnnIf ann_p ann_c ann_a)
            else tcFail $ IfBranchesTypeMismatch e tc ta
annExpr env e@(Let bindings body)
    = do Bindings bs' <- Traversable.mapM (annExpr env) bindings
         mapM_ check_binding_shape bs'
         let env' = [(x, t) | (x, (t, _)) <- bs']
         ann_body@(body_type, _) <- annExpr (env' ++ env) body
         return (body_type, AnnLet (Bindings bs') ann_body)
    where
      check_binding_shape (x, e'@(PrimTy (ValuesSh _), _))
          = tcFail $ ValuesBoundToVariable e x (stripAnnExpr e')
      check_binding_shape _
          = return ()
annExpr env (LetValues bindings body)
    = do Bindings bs' <- Traversable.mapM (annExpr env) bindings
         env' <- concatMapM destructure bs'
         ann_body@(body_type, _) <- annExpr (env' ++ env) body
         return (body_type, AnnLetValues (Bindings bs') ann_body)
    where
      destructure (xs, ann_e@(PrimTy (ValuesSh ss), _))
          | length xs == length ss
          = return $ zip xs (map PrimTy ss)
          | otherwise
          = tcFail $ PatternVarsNumMismatch xs (stripAnnExpr ann_e)
      destructure (_, ann_e@(t, _))
          = tcFail $ ShapeMismatch (stripAnnExpr ann_e) t "values"
annExpr env (Car e)
    = do ann_e@(t, _) <- annExpr env e
         case t of
           PrimTy (ConsSh s1 _) -> return (PrimTy s1, AnnCar ann_e)
           _                    -> tcFail $ ShapeMismatch e t "cons"
annExpr env (Cdr e)
    = do ann_e@(t, _) <- annExpr env e
         case t of
           PrimTy (ConsSh _ s2) -> return (PrimTy s2, AnnCdr ann_e)
           _                    -> tcFail $ ShapeMismatch e t "cons"
annExpr env (VectorRef e i)
    = do ann_e@(t, _) <- annExpr env e
         case t of
           PrimTy (VectorSh ss) -> return (PrimTy (ss !! i), AnnVectorRef ann_e i)
           _                    -> tcFail $ ShapeMismatch e t "vector"
annExpr env e@(Cons e1 e2)
    = do ann_e1@(t1, _) <- annExpr env e1
         ann_e2@(t2, _) <- annExpr env e2
         s1             <- fromPrimTy t1
         check_shape s1 e1
         s2             <- fromPrimTy t2
         check_shape s2 e2
         return (PrimTy (ConsSh s1 s2), AnnCons ann_e1 ann_e2)
    where
      check_shape (ValuesSh _) component
          = tcFail $ ValuesAsConstructorArg e component
      check_shape _ _
          = return ()
annExpr env e@(Vector es)
    = do es' <- mapM (annExpr env) es
         ss  <- mapM (fromPrimTy . fst) es'
         zipWithM_ check_shape ss es
         return (PrimTy (VectorSh ss), AnnVector es')
    where
      check_shape (ValuesSh _) component
          = tcFail $ ValuesAsConstructorArg e component
      check_shape _ _
          = return ()
annExpr env e@(Values es)
    = do es' <- mapM (annExpr env) es
         ss  <- mapM (fromPrimTy . fst) es'
         zipWithM_ check_shape ss es
         return (PrimTy (ValuesSh ss), AnnValues es')
    where
      check_shape (ValuesSh _) component
          = tcFail $ NestedValues e component
      check_shape _ _
          = return ()
annExpr env e@(ProcCall proc args)
    | Just proc_type@(ProcTy arg_shapes ret_shape) <- lookup proc env
    , let nargs       = length args
    , let narg_shapes = length arg_shapes
    = if nargs == narg_shapes
      then do ann_args <- mapM (annExpr env) args
              zipWithM_ check_arg_type arg_shapes ann_args
              return (PrimTy ret_shape, AnnProcCall proc ann_args)
      else tcFail $ ProcArgsNumMismatch proc proc_type nargs
    | otherwise
    = tcFail $ UndefinedProc proc
    where
      check_arg_type arg_shape ann_arg@(arg_type, _)
          | PrimTy (ValuesSh _) <- arg_type
          = tcFail $ ValuesUsedAsProcArg e (stripAnnExpr ann_arg)
          | PrimTy _ <- arg_type
          = return ()
          | otherwise
          = tcFail
          $ ProcArgTypeMismatch e (stripAnnExpr ann_arg) arg_type arg_shape

-- The declared type of a given procedure.
declProcType :: Defn -> Type
declProcType (Defn proc args body) = ProcTy arg_shapes ret_shape
    where
      ret_shape  = snd proc
      arg_shapes = map snd args

fromPrimTy :: Type -> TC Shape
fromPrimTy (PrimTy s) = return s
-- 'fromPrimTy' is only used where the following cannot happen.  We
-- add a catch-all case anyway.
fromPrimTy _          = fail "Procedure type where a shape is expected"
