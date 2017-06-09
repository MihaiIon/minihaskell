---------------------------------------------------------------------------
-- Fichier principal pour le TP 1
-- Vous avez à modifier / compléter les fonctions de ce fichier
---------------------------------------------------------------------------

module TypeCheck where

import Parseur
import Types
import Eval

---------------------------------------------------------------------------
-- Fonction pour la vérification de type
-- Vous allez devoir modifier typeCheck
---------------------------------------------------------------------------
type Tenv = [(Symbol, Type)]
tenv0 :: Tenv
tenv0 = [("+", TArrow TInt (TArrow TInt TInt)),
         ("-", TArrow TInt (TArrow TInt TInt)),
         ("*", TArrow TInt (TArrow TInt TInt))]

------------------------
-- Helpers
------------------------

isInTenv :: Symbol -> Tenv -> Bool
isInTenv sym [] = False
isInTenv sym ((s,_):xs) = if sym == s then True else (isInTenv sym xs)

addToTenv :: (Symbol, Type) -> Tenv -> Either Error Tenv
addToTenv (sym, t) env = 
  if (isInTenv sym env)
    then Left $ error $ "addToTenv :: '" ++ sym ++ "' is an invalid parameter name or is already defined"
    else Right ((sym, t):env)

------------------------

lookupType :: [(Symbol, Type)] -> Symbol -> Either Error Type
lookupType [] sym = Left $ "Not in scope variable : " ++ sym
lookupType ((s,v) : _) sym | s == sym = Right v
lookupType (_ : xs) sym = lookupType xs sym

typeCheck :: Tenv -> Exp -> Either Error Type
typeCheck _ (EInt _) = Right TInt
typeCheck env (EVar sym) = lookupType env sym

typeCheck env (EApp e1 e2) = do
  r1 <- typeCheck env e1
  r2 <- typeCheck env e2
  case r1 of 
    TArrow (TArrow a z) r -> 
      if z == r2 
        then return (TArrow a r) 
        else return $ error "TypeCheck :: Error in EApp, 1st case."
    TArrow a b -> 
      if a == r2 
        then return b 
        else return $ error $ "TypeCheck :: Error in EApp, 2nd case." 
    t -> return t

typeCheck env (ELam sym t body) = do
  env' <- addToTenv (sym, t) env
  r <- typeCheck env' body
  case r of
    TArrow a b -> return $ TArrow a (TArrow t r)
    t' -> return $ TArrow t' (TArrow t t') 


typeCheck env (ELet lenv body) = do
  t <- typeCheck ((buildEnv [] lenv)++env) body
  return t
  where buildEnv :: Tenv -> LetEnv -> Tenv
        buildEnv env [] = env
        buildEnv env ((s,t,_):xs) = buildEnv ((s,t):env) xs


-- TO DO  ( REWORK )
{-typeCheck env (ELet sym t v e) = do
  env' <- addToTenv (sym, t) env
  r <- typeCheck env' e -- If an error occurs, it will occur here.
  case e of 
    ELet _ _ _ _-> Right $ TArrow t r
    otherwise -> Right $ TArrow t TInt -- We know that in all cases, a lambda will return a Int. -}