module Example.Basis.Main
    ( outFunc
    ) where

import Core.SMT ( Expr(..), Formula (..), Command (..) )

-- Create variables

outFunc :: IO ()
outFunc = do
    putStrLn "hello, basis"