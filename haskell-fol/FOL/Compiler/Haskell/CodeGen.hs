-- ----------------------------------------------------------------------
-- Copyright 2010-2011 National University of Ireland.
-- ----------------------------------------------------------------------
-- This file is part of DysVunctional Language.
-- 
-- DysVunctional Language is free software; you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
--  License, or (at your option) any later version.
-- 
-- DysVunctional Language is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU Affero General Public License
-- along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
-- ----------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Compiler.Haskell.CodeGen where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.TypeCheck
import FOL.Language.Parser
import FOL.Language.Pretty
import FOL.Compiler.Haskell.Syntax

import Data.Generics.Uniplate.Data

translateType :: Type -> HsType
translateType (PrimTy shape) = HsPrimType (translateShape shape)
translateType (ProcTy arg_shapes ret_shape)
    = HsFuncType (map translateShape arg_shapes) (translateShape ret_shape)

translateShape :: Shape -> HsShape
translateShape NilSh  = HsUnitSh
translateShape BoolSh = HsBoolSh
translateShape RealSh = HsUnboxedDoubleSh
translateShape (ConsSh shape1 shape2)
    = HsPairSh (boxShape $ translateShape shape1)
               (boxShape $ translateShape shape2)
translateShape (VectorSh shapes)
    = error "translateShape: vector shapes are not supported yet"
translateShape (ValuesSh shapes)
    = HsUnboxedTupleSh (map translateShape shapes)
translateShape FunctionSh = HsFunctionSh

boxShape :: HsShape -> HsShape
boxShape HsUnboxedDoubleSh = HsBoxedDoubleSh
boxShape shape = shape

boxType :: HsType -> HsType
boxType (HsPrimType t) = HsPrimType (boxShape t)
boxType t = t

compileProg :: Name -> AnnProg Type -> [HsSCDefn]
compileProg export_name (prog_type, AnnProg defns expr)
    = map compileDefn defns ++ [entry_point]
    where
      entry_point
          = HsSCDefn (boxType $ translateType prog_type) export_name [] (compileExprBoxed expr)

compileDefn :: AnnDefn Type -> HsSCDefn
compileDefn (proc_type, AnnDefn proc args expr)
    = HsSCDefn (translateType proc_type)
      proc_name arg_names (compileExpr expr)
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
compileExpr' (AnnIf p c a)
    = HsIf (compileExpr p) (compileExpr c) (compileExpr a)
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
compileExpr' (AnnCons e1 e2) = HsPair (compileExprBoxed e1)
                                      (compileExprBoxed e2)
compileExpr' (AnnVector _)
    = error "compileExpr': vector form is not supported yet"
compileExpr' (AnnValues es)
    = HsUnboxedTuple (map compileExpr es)
compileExpr' (AnnLambda x e)
    = HsFuncAppl (Name "Function") [HsLambda x (compileExpr e)]
compileExpr' (AnnProcCall name args) = HsFuncAppl name (map compileExpr args)

compileExprBoxed :: AnnExpr Type -> HsExpr
compileExprBoxed (PrimTy RealSh, e) = HsFuncAppl (Name "D#") [compileExpr' e]
compileExprBoxed (_, e) = compileExpr' e

lambdasInProg :: AnnProg Type -> [Shape]
lambdasInProg (_, AnnProg defns expr)
    = concatMap lambdasInDefn defns ++ lambdasInExpr expr

lambdasInDefn :: AnnDefn Type -> [Shape]
lambdasInDefn (_, AnnDefn _ _ body) = lambdasInExpr body

lambdasInExpr :: AnnExpr Type -> [Shape]
lambdasInExpr (_, e) = [s | AnnLambda x (PrimTy s, _) <- universe e]

compileProgAsModule :: Name -> Name -> Prog -> HsModule
compileProgAsModule module_name export_name prog
    = HsModule pragmas
               module_name
               [export_name]
               [HsImport "GHC.Exts"]
               ty_defns
               sc_defns
    where
      pragmas  = [HsPragma "{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}"]
      sc_defns = prelude ++ compileProg export_name ann_prog
      ann_prog = ann prog
      ty_defns = [HsTyDefn (translateShape s) | s <- lambdasInProg ann_prog]

program = parse "(begin (define (operation-19 the-closure-141 the-closure-142 the-formals-143 the-formals-144) (argument-types real real real real real) (if (< (abs (- the-formals-143 the-formals-144)) .00001) the-formals-144 (operation-19 the-closure-142 the-closure-142 the-formals-144 (/ (+ the-formals-144 (/ the-closure-141 the-formals-144)) 2)))) (let ((the-formals-92 (real 2))) (let ((anf-79 (real 1.))) (cons 1.4142135623730951 (operation-19 the-formals-92 the-formals-92 anf-79 (/ (+ anf-79 (/ the-formals-92 anf-79)) 2))))))"

program2 = parse "(begin (define (operation-19 the-formals-95) (argument-types real bool) (if (= the-formals-95 0) #t (let ((the-formals-99 (- the-formals-95 1))) (if (= the-formals-99 0) #f (operation-19 (- the-formals-99 1)))))) (operation-19 (real 5)))"

program3 = parse "(begin (define (operation-363 the-formals-7993 the-formals-7994 the-formals-7995) (argument-types real real real (values real real)) (if (= the-formals-7995 0) (values the-formals-7993 the-formals-7994) (let-values (((the-formals-8282 the-formals-8283) (operation-423 (real .001) the-formals-7994))) (operation-363 (operation-576 the-formals-7993 the-formals-8282) (operation-576 the-formals-7994 the-formals-8283) (+ the-formals-7995 -1))))) (define (operation-423 the-formals-9125 the-formals-9127) (argument-types real real (values real real)) (values the-formals-9125 (* the-formals-9125 the-formals-9127))) (define (operation-576 the-formals-9141 the-formals-9142) (argument-types real real real) (+ the-formals-9141 the-formals-9142)) (let-values (((receipt-7721 receipt-7722) (operation-363 (real 0) (real 1) (real 1000)))) (cons receipt-7721 receipt-7722)))"
