module Example.KillerSudoku.Main
    ( testFunc
    ) where

import System.Environment ( getArgs )
import Data.List ( transpose )

import Core.SMT ( Expr(..), Formula (..), Command (..) )


testFunc :: IO ()
testFunc = do
    process ["./src/Example/KillerSudoku/cage_constraints"]

consoleFunc :: IO ()
consoleFunc = do
    -- Read cage file
    args <- getArgs
    -- Start processing to generate SMTLib file
    process args


process :: [String] -> IO ()
process [filename] = do
    -- Read cage contraints
    cageInput <- readFile filename
    -- Declare variables
    putStr declareVars
    -- Assert constraints
    putStrLn $ assertConstraints cageInput
    -- Check commands
    putStrLn check


-- Declare variables
declareVars :: String
declareVars = unlines $ map (show . Declare) $ concat vars

-- Generate constraints
generate :: String -> [Formula]
generate cageInput = digitConstraints ++ rowConstraints ++ colConstrains ++ gridConstraints ++ cageConstraints cageInput

-- Assert constraints
assertConstraints :: String -> String
assertConstraints cageInput = show (Assert (And (generate cageInput)))

-- Check commands
check :: String
check = unlines $ map show [Check, GetVal $ concat vars]


-- Define length of the side
sideLength :: Int
sideLength = 9

-- Define length of a grid
gridLength :: Int
gridLength = 3

-- Create variables [[x11, x12, ...], [x21, x22, ...], ...]
vars :: [[Expr]]
vars = [[Var ("x" ++ show r ++ show c) | c <- [1..sideLength]] | r <- [1..sideLength]]

-- Digit constraints
digitConstraints :: [Formula]
digitConstraints = [Leq (Val 1) v | v <- varList] ++ [Leq v (Val sideLength) | v <- varList]
    where varList = concat vars

-- Row constraints
rowConstraints :: [Formula]
rowConstraints = [Distinct row | row <- vars]

-- Column constraints
colConstrains :: [Formula]
colConstrains = [Distinct col | col <- transpose vars]

-- 3x3 grid constraints
gridConstraints :: [Formula]
gridConstraints = [Distinct grid | grid <- grids]
    where grids = [[vars!!r'!!c' | r' <- [r..(r+gridLength-1)], c' <- [c..(c+gridLength-1)]] | r <- [0,gridLength..(sideLength - 1)], c <- [0,gridLength..(sideLength - 1)]]

-- Cage constraints
cageConstraints :: String -> [Formula]
cageConstraints cageInput = [Eq (Val (read v)) (Plus (map (\x -> Var ("x" ++ x)) xs)) | (v:xs) <- cages]
    where cages = map words (lines cageInput)
