module Sexp where

import Parseur
import Types

------------------------
-- Helpers
------------------------

---------------
isSpecialKeyword :: Sexp -> Bool
isSpecialKeyword (SSym s) = if s `elem` (["+","-","*"] ++ reservedKeywords) 
  then True else False
isSpecialKeyword (SNum n) = True
isSpecialKeyword _ = True

---------------
isInLetEnv :: Sexp -> LetEnv -> Bool
isInLetEnv sym [] = -- Check for forbbiden variable names.
  if (isSpecialKeyword sym) then True else False

isInLetEnv (SSym sym) (((EVar s),_,_):xs) = 
  if sym == s then True else (isInLetEnv (SSym sym) xs)

---------------
addToLetEnv :: (Sexp, Sexp, Sexp) -> LetEnv -> Either Error LetEnv
addToLetEnv ((SSym sym), t, v) env = 
  if (isInLetEnv (SSym sym) env)
    then Left $ error $ "addToLetEnv :: '" ++ sym ++ "' is an invalid parameter name or is already defined"
    else do
      t' <- sexp2type t
      v' <- parse v
      return $ (((EVar sym), t', v'):env)
      where parse :: Sexp -> Either Error Exp
            parse (SNum n) = Right $ EInt n
            parse (SSym s) = if (isSpecialKeyword (SSym s)) 
              then Left $  error $ "addToLetEnv :: '" ++ s ++ "' is an invalid parameter name or is already defined."
              else Right $ EVar s
addToLetEnv _ _ = Left $ error "addToLetEnv :: 'arg' or 'env' are malformed."

------------------------

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
--------------------------------------------
sexp2Exp (SList ((SSym "let") : args : body : [])) = do
  case args of
    (SList []) -> return $ error "sexp2Exp :: 'let' is missing arguments."
    otherwise -> do
      env <- buildEnv [] args
      body' <- sexp2Exp body
      return $ ELet env body'
      where buildEnv :: LetEnv -> Sexp -> Either Error LetEnv
            buildEnv env (SList []) = Right env
            buildEnv env (SList ((SList (sym : t : v : [])) : xs)) = do
              env' <- addToLetEnv (sym, t, v) env
              fenv <- buildEnv env' (SList xs)
              return fenv
            buildEnv _ _ = Left $ error "sexp2Exp :: 'let' arguments are malformed."

-- TRY THIS in 'MAIN>'
-- sexp2Exp (SList ((SSym "let") : (SList ((SList ((SSym "y") : (SSym "Int") : (SNum 4) : [])) : (SList ((SSym "x") : (SSym "Int") : (SSym "y") : [])) : [])) :  (SSym "x") : []))


-- LAMBDA
--------------------------------------------
sexp2Exp (SList ((SSym "lambda") :
                 (SList ((SList ((SSym var) : t : [])) : [])) :
                 body :
                 [])) = do
  body' <- sexp2Exp body
  t' <- sexp2type t
  return $ ELam var t' body'


-- LAMBDA - Multiple arguments
--------------------------------------------
-- If there are multiple arguments, split the arguments and
-- create sub lambda expressions with each one argument.
sexp2Exp (SList ((SSym s) : (SList (x:xs)) : body : [])) | s <- "lambda" =
  let body' = SList ((SSym s) :
                      (SList xs) :
                      body : [])
  in do 
    r <- sexp2Exp (SList ((SSym s) : 
                          (SList (x:[])) :
                          body' : []))
    return r

-- ERROR.
sexp2Exp (SList ((SSym s) : (SList []) : _ : [])) | s `elem` ["lambda", "let"] = 
  Left "Syntax Error : No parameter"


-- Default case : One argument.
----------------------------------------------------
sexp2Exp (SList (func : arg : [])) = do
  func' <- sexp2Exp func
  arg' <- sexp2Exp arg
  return $ EApp func' arg'

-- If there are multiple arguments, create a growing left-side ASA.
-- The arguments are always on the right.
sexp2Exp (SList (func : x : xs)) = do
  r1 <- sexp2Exp (SList (func:xs))  -- recursion
  r2 <- sexp2Exp x                  -- arg
  return $ EApp r1 r2
  
sexp2Exp _ = Left "Syntax Error : Ill formed Sexp"


{-
-- LET
--------------------------------------------
sexp2Exp (SList ((SSym "let") : 
                 (SList ((SList ((SSym var ) : t : v : [])) : [])) :
                 body :
                 [])) = do
  body' <- sexp2Exp body
  t' <- sexp2type t
  v' <- sexp2Exp v
  return $ ELet var t' v' body'


-- LAMBDA
--------------------------------------------
sexp2Exp (SList ((SSym "lambda") :
                 (SList ((SList ((SSym var) : t : [])) : [])) :
                 body :
                 [])) = do
  body' <- sexp2Exp body
  t' <- sexp2type t
  return $ ELam var t' body'


-- General
--------------------------------------------
-- If there are multiple arguments, split the arguments and
-- create sub lambda expressions with each one argument.
sexp2Exp (SList ((SSym s) : (SList (x:xs)) : body : [])) | s `elem` ["lambda", "let"] =
  let body' = SList ((SSym s) :
                      (SList xs) :
                      body : [])
  in do 
    r <- sexp2Exp (SList ((SSym s) : 
                          (SList (x:[])) :
                          body' : []))
    return r

-- ERROR.
sexp2Exp (SList ((SSym s) : (SList []) : _ : [])) | s `elem` ["lambda", "let"] = 
  Left "Syntax Error : No parameter"

-}
