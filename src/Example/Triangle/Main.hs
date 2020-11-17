module Example.Triangle.Main
    ( outFunc
    ) where

import Core.SMT ( Expr(..), Formula (..), Command (..) )

-- Three sides of a right triangle
a :: Expr
a = Var "a"

b :: Expr
b = Var "b"

c :: Expr
c = Var "c"

-- Pythagoras theorem
-- a^2 + b^2 = c^2, where a, b, c \in Nat
rTriangle :: Formula
rTriangle = And [pythagoras, allNat]
    where   pythagoras = Eq (Plus [Mul [a, a], Mul [b, b]]) (Mul [c, c])
            allNat = And [Gt a v0, Gt b v0, Gt c v0]
            v0 = Val 0

-- a + b > c
property1 :: Formula
property1 = Gt (Plus [a, b]) c

-- c - a < b & c - b < a
property2 :: Formula
property2 = And [Lt (Sub [c, a]) b, Lt (Sub [c, b]) a]


outFunc :: IO ()
outFunc = do
    -- Output variable declarations
    putStrLn $ unlines $ map (show . Declare) [a, b, c]
    -- Assert a right triangle
    print $ Assert rTriangle
    -- Assert property (a + b > c)
    print $ Assert property1
    -- Assert property (c - a < b & c - b < a)
    print $ Assert property2
    -- Check satisfiability
    print Check
    -- Get models
    print GetMod