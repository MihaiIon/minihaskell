module Types where

import Parseur

---------------------------------------------------------------------------
-- Le datatype des types
---------------------------------------------------------------------------
data Type = TInt
          | TArrow Type Type
          | TData Symbol
          deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show (TArrow t1 t2) = showParen' t1 ++ " -> " ++ show t2
    where showParen' x@(TArrow _ _) = "(" ++ show x ++ ")"
          showParen' x = show x
  show (TData sym) = sym

---------------------------------------------------------------------------
-- Le datatype des expressions et valeurs
---------------------------------------------------------------------------

type LetEnv = [(Symbol, Type, Exp)]
type CaseEnv = [(Value,Exp)]

type Cons = (Symbol, [Type])
type Decl = (Symbol, [Cons])

data Exp = EInt Int
         | EVar Symbol
         | EApp Exp Exp
         | ELam Symbol Type Exp
         | ELet LetEnv Exp -- Let - [(Symbol, Type, Exp)] - Body
         | EData [Decl] Exp
         | ECase Type CaseEnv -- Type [({VCons|VSym}, Exp)]
         deriving (Eq,Show)

data Value = VInt Int
           | VLam Symbol Exp Env
           | VPrim (Value -> Value)
           | VCons Symbol [Type] -- VCons Symbol [VSym]

instance Show Value where
  show (VInt n) = show n
  show (VCons sym vs) = 
    sym ++ "(" ++ (show' vs) ++ ")"
    where show' :: [Type] -> String
          show' (x:[]) = show x
          show' (x:xs) = (show x) ++ " " ++ (show' xs)
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