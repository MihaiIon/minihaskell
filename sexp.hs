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



sexp2Exp (SList ((SSym "lambda") :
                 (SList ((SList ((SSym var) : t : [])) : [])) :
                 body :
                 [])) = do
  body' <- sexp2Exp body
  t' <- sexp2type t
  return $ ELam var t' body'


sexp2Exp (SList ((SSym "lambda") :
                 (SList []) :
                 _ :
                 [])) = Left "Syntax Error : No parameter"
      
sexp2Exp (SList (func : arg : [])) = do
  func' <- sexp2Exp func
  arg' <- sexp2Exp arg
  return $ EApp func' arg'
  
sexp2Exp _ = Left "Syntax Error : Ill formed Sexp"