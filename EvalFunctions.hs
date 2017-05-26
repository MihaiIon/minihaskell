---------------------------------------------------------------------------
-- Fichier principal pour le TP 1
-- Vous avez à modifier / compléter les fonctions de ce fichier
---------------------------------------------------------------------------

module EvalFunctions where

import Parseur
import EvalTypes
import EvalSex

---------------------------------------------------------------------------
-- Fonction d'évaluation
-- Vous allez devoir modifier eval
---------------------------------------------------------------------------

lookupVar :: [(Symbol, Value)] -> Symbol -> Value
lookupVar [] sym = error "Oups ..."
lookupVar ((s,v) : _) sym | s == sym = v
lookupVar (_ : xs) sym = lookupVar xs sym

eval :: Env -> Exp -> Value
eval _ (EInt x) = VInt x
eval env (EVar sym) = lookupVar env sym
--added
eval env (Elan sym t exp) = VLam sym exp env
eval env (EApp e1 e2) = (eval env e1) (eval env e2) 
--
eval _ _ = error "Oups ..."