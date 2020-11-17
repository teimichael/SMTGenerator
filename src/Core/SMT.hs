module Core.SMT
    ( Expr (..),
      Formula (..),
      Command (..)
    ) where

-- Define expression structure
data Expr =
      Val Int
    | Var String
    | Plus [Expr]

-- Define formula structure
data Formula =
      And [Formula]
    | Or [Formula]
    | Eq Expr Expr
    | Geq Expr Expr
    | Leq Expr Expr
    | Distinct [Expr]

-- Define command structure
data Command = 
      Declare Expr
    | Assert Formula
    | Check
    | GetVal [Expr]


instance Show Expr where
  show (Val v) = show v
  show (Var v) = v
  show (Plus es) = "(+ " ++ unwords (map show es) ++ ")"


instance Show Formula where
  show (And fs) = "(and " ++ unwords (map show fs) ++ ")"
  show (Or fs) = "(or " ++ unwords (map show fs) ++ ")"
  show (Eq e0 e1) = "(= " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Geq e0 e1) = "(>= " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Leq e0 e1) = "(<= " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Distinct es) = "(distinct " ++ unwords (map show es) ++ ")"
  

instance Show Command where
  show (Declare (Var v)) = "(declare-fun " ++ v ++ " () Int)"
  show (Assert f) = "(assert " ++ show f ++ ")"
  show Check = "(check-sat)"
  show (GetVal es) = "(get-value (" ++ unwords (map show es) ++ "))"