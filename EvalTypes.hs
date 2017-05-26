module EvalTypes where

import Parseur


---------------------------------------------------------------------------
-- Le datatype des types
---------------------------------------------------------------------------
data Type = TInt
          | TBool -- we want a error on : typeCheck tenv0 (ELam "op" TInt (EApp (EApp (EVar "+") (EInt 1)) (EBool True)))
          | TArrow Type Type
          deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show (TArrow t1 t2) = showParen' t1 ++ " -> " ++ show t2
    where showParen' x@(TArrow _ _) = "(" ++ show x ++ ")"
          showParen' x = show x

---------------------------------------------------------------------------
-- Le datatype des expressions et valeurs
---------------------------------------------------------------------------
data Exp = EInt Int
         | EBool Bool
         | EVar Symbol
         | EApp Exp Exp
         | ELam Symbol Type Exp
         deriving (Eq,Show)

data Value = VInt Int
           | VBool Bool
           | VLam Symbol Exp Env
           | VPrim (Value -> Value)

instance Show Value where
  show (VInt n) = show n
  show (VBool b)= show b
  show _ = "<function>"

instance Eq Value where
  (VInt n1) == (VInt n2) = n1 == n2
  -- Impossible de comparer fonctions et primitives
  _ == _ = False


---------------------------------------------------------------------------
-- Pour ce TP, une erreur est simplement un chaîne de caractères
-- expliquant le problème
---------------------------------------------------------------------------
type Error = String


---------------------------------------------------------------------------
-- L'environnement d'exécution
-- Une simple liste qui contient des identifiants associés à des valeurs
---------------------------------------------------------------------------
type Env = [(Symbol, Value)]

env0 :: Env
env0 = [("+", prim (+)),
        ("-", prim (-)),
        ("*", prim (*))]
  where prim op =
          VPrim (\ (VInt x) -> VPrim (\ (VInt y) -> VInt (x `op` y)))