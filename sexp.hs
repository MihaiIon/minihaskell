module Sexp where

import Parseur
import Types

---------------------------------------------------------------------------
-- Fonction de converstion des Sexp en Expressions (Exp)
-- Vous allez devoir modifier sexp2type et sexp2Exp
---------------------------------------------------------------------------
sexp2type :: Sexp -> Either Error Type
sexp2type (SSym "Int") = Right TInt
sexp2type (SList (x : xs)) = do
  type1 <- sexp2type x
  type2 <- sexp2type (SList xs)
  return $ TArrow type1 type2
sexp2type _ = Left "Ill formed type"

reservedKeywords :: [Symbol]
reservedKeywords = ["lambda", "let", "case", "data", "Erreur"]

sexp2Exp :: Sexp -> Either Error  Exp
sexp2Exp (SNum x) = Right $ EInt x
sexp2Exp (SSym ident) | ident `elem` reservedKeywords
  = Left $ ident ++ " is a reserved keyword"
sexp2Exp (SSym ident) = Right $ EVar ident


-- LET
sexp2Exp (SList ((SSym "let") : 
                 (SList ((SList ((SSym var ) :t : v : [])) :[] )):
                 body :
                 [])) = do
  body' <- sexp2Exp body
  t' <- sexp2type t
  v' <- sexp2Exp v
  return $ ELet var t' body'
  

sexp2Exp (SList ((SSym "let") : (SList(x:xs)) : body : [])) =
  let body' = SList ((SSym "let") :
                     (SList xs) :
                     body :[])
  in do 
    r <- sexp2Exp (SList ((SSym "let") : 
                          (SList (x:[])) : 
                          body' : []))
    return r


sexp2Exp (SList ((SSym "let") :
                 (SList []) :
                 _ :
                 [])) = Left "Syntax Error : No parameter"
--------------------------------------------
--LAMBDA

sexp2Exp (SList ((SSym "lambda") :
                 (SList ((SList ((SSym var) : t : [])) : [])) :
                 body :
                 [])) = do
  body' <- sexp2Exp body
  t' <- sexp2type t
  return $ ELam var t' body'

-- If there are multiple arguments, split the arguments and
-- create sub lambda expressions with each one argument.
sexp2Exp (SList ((SSym "lambda") : (SList (x:xs)) : body : [])) =
  let body' = SList ((SSym "lambda") :
                      (SList xs) :
                      body : [])
  in do 
    r <- sexp2Exp (SList ((SSym "lambda") : 
                          (SList (x:[])) :
                          body' : []))
    return r

sexp2Exp (SList ((SSym "lambda") :
                 (SList []) :
                 _ :
                 [])) = Left "Syntax Error : No parameter"
----------------------------------------------------
-- 
sexp2Exp (SList (func : arg : [])) = do
  func' <- sexp2Exp func
  arg' <- sexp2Exp arg
  return $ EApp func' arg'

-- If there are multiple arguments
sexp2Exp (SList (func : x : xs)) = do
  r1 <- sexp2Exp (SList (func:xs))
  r2 <- sexp2Exp x
  return $ EApp r1 r2
  
sexp2Exp _ = Left "Syntax Error : Ill formed Sexp"