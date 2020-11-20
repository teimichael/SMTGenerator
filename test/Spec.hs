import Example.Triangle.Main
import Example.KillerSudoku.Main
import Example.Termination.Main

main :: IO ()
main = do
    putStrLn "Triangle Property Prover"
    Example.Triangle.Main.outFunc
    putStrLn "Killer Sudoku Solver"
    Example.KillerSudoku.Main.testFunc
    putStrLn "TRS Termination Prover"
    Example.Termination.Main.testFunc
