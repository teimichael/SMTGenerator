module Example.Termination.TRS
    ( Term (..),
      Rule (..),
      TRS (..)
    ) where

import Data.List ( intercalate )

data Term = V String -- V value
          | F String [Term] -- F symbol args
          deriving Eq

type Rule = (Term, Term)

type TRS = [Rule]

instance Show Term where
  show (V v) = v
  show (F s as) = s ++ "(" ++ intercalate "," (map show as) ++ ")"