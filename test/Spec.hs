import Core.SMT ( Expr(..), Formula (..), Command (..) )

a :: Expr
a = Var "x"

b :: Expr
b = Var "y"

c :: Expr
c = Var "z"

n10 :: Expr
n10 = Val 10


eq :: Formula
eq = Eq (Plus [a, b, c]) n10


declare :: Command
declare = Declare a

assert :: Command
assert = Assert eq

main :: IO ()
main = print declare
