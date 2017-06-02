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
lookupVar [] sym = error $ "'"++sym++"' is not in the function's scope"
lookupVar ((s,v) : _) sym | s == sym = v
lookupVar (_ : xs) sym = lookupVar xs sym

eval :: Env -> Exp -> Value
eval _ (EInt x) = VInt x
eval _ (EBool b)= VBool b -- we want a error on : EBool 5
eval env (EVar sym) = lookupVar env sym

eval env (ELam sym t e) = (VLam sym e env)
--eval env (EApp e1 e2) = do
--  r1 <- eval e1
--  r2 <- eval e2
--  return VPrim \
eval _ _ = error "eval Oups ..."

