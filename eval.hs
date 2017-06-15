---------------------------------------------------------------------------
-- Fichier principal pour le TP 1
-- Vous avez à modifier / compléter les fonctions de ce fichier
---------------------------------------------------------------------------

module Eval where

import Parseur
import Types
import Sexp

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
eval env (EVar sym) = lookupVar env sym
eval env (EApp e1 e2) = 
  let r = eval env e1 -- VLam or VPrim
      v = eval env e2 -- VInt
  in case r of
    VPrim f -> f v
    VLam sym e env -> eval ((sym,v):env) e

eval env (ELam sym _ e) = (VLam sym e env)

eval env (ELet lenv body) = 
  let f = \(sym, _, e) -> (sym ,(eval env' e)) -- Create env tuple.
      env' = (map f lenv) ++ env
  in eval env' body

eval env (EData types e) = 
  let f = \v -> 
        case v of
          VData (TData sym) v -> (sym, v)
      env' = (map f types) ++ env
  in eval env' e