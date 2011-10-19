{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Compiler.Haskell.CodeGen where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.TypeCheck
import FOL.Language.Parser
import FOL.Language.Pretty
import FOL.Compiler.Haskell.Syntax

translateType :: Type -> HsType
translateType (PrimTy shape) = HsPrimType (translateShape shape)
translateType (ProcTy arg_shapes ret_shape)
    = HsFuncType (map translateShape arg_shapes) (translateShape ret_shape)

translateShape :: Shape -> HsShape
translateShape NilSh = HsUnitSh
translateShape BoolSh = HsBoolSh
translateShape RealSh = HsRealSh
translateShape (ConsSh shape1 shape2)
    = HsPairSh (translateShape shape1) (translateShape shape2)
translateShape (VectorSh shapes)
    = error "translateShape: vector shapes are not supported yet"
translateShape (ValuesSh shapes)
    = HsUnboxedTupleSh (map translateShape shapes)

compileProg :: AnnProg Type -> [HsSCDefn]
compileProg (prog_type, AnnProg defns expr)
    = map compileDefn defns ++ [entry_point]
    where
      entry_point
          = HsSCDefn (translateType prog_type) (Name "__main__") [] (compileExpr expr)

compileDefn :: AnnDefn Type -> HsSCDefn
compileDefn (proc_type, AnnDefn proc args expr)
    = HsSCDefn (translateType proc_type) proc_name arg_names (compileExpr expr)
    where
      proc_name = fst proc
      arg_names = map fst args

compileExpr :: AnnExpr Type -> HsExpr
compileExpr (_, e) = compileExpr' e

compileExpr' :: AnnExpr' Type -> HsExpr
compileExpr' (AnnVar x) = HsVar x
compileExpr' AnnNil = HsUnit
compileExpr' (AnnBool b) = HsBool b
compileExpr' (AnnReal r) = HsReal r
compileExpr' (AnnIf p c a) = HsIf (compileExpr p) (compileExpr c) (compileExpr a)
compileExpr' (AnnLet (Bindings bs) body)
    -- Haskell `let' is actually recursive, but we alpha-rename the
    -- program before compiling, so no recursion in bindings can
    -- occur.
    = HsLet (Bindings bs') (compileExpr body)
    where
      bs' = [(HsPatVar x, compileExpr e) | (x, e) <- bs]
compileExpr' (AnnLetValues (Bindings bs) body)
    = HsLet (Bindings bs') (compileExpr body)
    where
      bs' = [(HsPatTuple xs, compileExpr e) | (xs, e) <- bs]
compileExpr' (AnnCar e) = HsFst (compileExpr e)
compileExpr' (AnnCdr e) = HsSnd (compileExpr e)
compileExpr' (AnnVectorRef _ _)
    = error "compileExpr': vector-ref form is not supported yet"
compileExpr' (AnnCons e1 e2) = HsPair (compileExpr e1) (compileExpr e2)
compileExpr' (AnnVector _)
    = error "compileExpr': vector form is not supported yet"
compileExpr' (AnnValues es)
    = HsUnboxedTuple (map compileExpr es)
compileExpr' (AnnProcCall name args) = HsFuncAppl name (map compileExpr args)

compileProgAsModule :: Name -> Prog -> HsModule
compileProgAsModule module_name prog
    = HsModule pragmas
               module_name
               [Name "__main__"]
               [HsImport "GHC.Exts"]
               sc_defns
    where
      pragmas  = [HsPragma "{-# LANGUAGE MagicHash, UnboxedTuples #-}"]
      sc_defns = prelude ++ compileProg (ann prog)

program = parse "(begin (define (operation-19 the-closure-141 the-closure-142 the-formals-143 the-formals-144) (argument-types real real real real real) (if (< (abs (- the-formals-143 the-formals-144)) .00001) the-formals-144 (operation-19 the-closure-142 the-closure-142 the-formals-144 (/ (+ the-formals-144 (/ the-closure-141 the-formals-144)) 2)))) (let ((the-formals-92 (real 2))) (let ((anf-79 (real 1.))) (cons 1.4142135623730951 (operation-19 the-formals-92 the-formals-92 anf-79 (/ (+ anf-79 (/ the-formals-92 anf-79)) 2))))))"