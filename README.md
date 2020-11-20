# SMTGenerator
Writing SMT-LIB Standard programs with Haskell.

## Examples
### Triangle Property
*Triangle.Main*
- **In**: Triangle with properties
- **Out**: SMT-LIB program

### Killer SudoKu Puzzle
*KillerSudoku.Main*
- **In**: cage constraints
- **Out**: SMT-LIB program

```
<*> cage_constraints > output
z3 output
```
> <*>: compiled program | "stack run"

### TRS Termination Prover
*Termination.Main*
(Polynomial interpretation method)
> *Lankford, D. S. (1979). On proving term rewriting systems are Noetherian. Memo MTP-3.*

- **In**: term rewriting system
- **Out**: SMT-LIB program (*sat -> termination*)
