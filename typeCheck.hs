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

-- Fonction additionnelle pour récupérer les symboles réservés.
getEnvSymbols :: [(Symbol, Type)] -> [Symbol]
getEnvSymbols [] = []
getEnvSymbols ((s,v):xs) = (s:(getEnvSymbols xs))

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
  Right $ TArrow r1 r2

typeCheck env (ELam sym t e) = 
  if sym `elem` (getEnvSymbols env)
    then Left $ error $ "'" ++ sym ++ "' is not a valid parameter name or is already defined"
    else do
      r <- typeCheck ((sym,t):env) e -- If error occurs, it will occur here.
      case e of 
        ELam _ _ _ -> Right $ TArrow t r
        otherwise -> Right $ TArrow t TInt -- We know that in all cases, a lambda will return a Int.