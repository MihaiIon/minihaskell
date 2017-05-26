---------------------------------------------------------------------------
-- Fichier principal pour le TP 1
-- Vous avez à modifier / compléter les fonctions de ce fichier
---------------------------------------------------------------------------

module Eval where

import Parseur
import EvalTypes
import EvalFunctions


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
typeCheck _ (EBool _)= Right TBool
typeCheck env (EVar sym) = lookupType env sym
<<<<<<< HEAD
--added
--typeCheck env (EApp (EVar op) (EInt x)) | op elem ["+","-","*"] = Right $ TArrow (typeCheck env op) TInt
--typeCheck env (EApp e1 e2) = Right $ TArrow (typeCheck env e1) (typeCheck env e2)
--
typeCheck _ _ = error "Oups ..."
=======
typeCheck env (EApp e1 e2) = do
  r1 <- typeCheck env e1
  r2 <- typeCheck env e2
  Right $ TArrow r1 r2
typeCheck env (ELam sym t e) = 
  if sym `elem` (getEnvSymbols env)
    then Left $ error $ "'" ++ sym ++ "' is not a valid parameter name"
    else do
      r <- typeCheck env e
      Right $ TArrow t r
>>>>>>> 58b21f4b24a730cb13145975c3d143d2a7270a41
