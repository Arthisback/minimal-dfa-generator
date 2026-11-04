# Minimal DFA Generator

Haskell project that reads tokens and regular grammars (RGs), builds a single NFA, determinizes it, minimizes the DFA (without equivalence-class partitioning), and finally adds an explicit error state.

## Features

- Parses an input file with:
  - token lines
  - grammar lines in the form `<S> ::= a<A> | ε`
- Builds **one combined NFA**:
  - shared initial state `S`
  - token paths with fresh states
  - grammar transitions merged into the same automaton
- Builds DFA using subset construction
- Minimizes DFA using a reverse/determinize strategy (Brzozowski-style)
- Adds explicit `ERR` sink state after minimization:
  - all missing transitions point to `ERR`
  - `ERR` loops to itself for all symbols
- Prints aligned transition tables for:
  - NFA
  - DFA
  - Minimized DFA
  - Minimized DFA + Error state

## Input Format

Each non-empty line is either:

1. **Token** (line does not start with `<`)
2. **Grammar production** (line starts with `<`)

### Grammar syntax

```
<NonTerminal> ::= a<OtherNonTerminal> | b | ε
```

Supported right-hand alternatives:

- `a<NT>` terminal + destination non-terminal
- `a` terminal-only transition
- `ε` epsilon (marks current non-terminal state as final)

## Example Input

```txt
ok
<S> ::= o<K> | a<A> | ε
<K> ::= k
<A> ::= b<A>
<Z> ::= x<Z> | ε
```

## Running

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- `runghc` available in PATH

### Execute

```bash
runghc Main.hs input-file.txt
```

You can use [input-file.txt](/home/arthur1/Projects/minimal-dfa-generator/minimal-dfa-generator/input-file.txt) as a starting point.

## Expected Output (Example)

The program prints 4 transition tables in sequence:

1. `NFA`
2. `DFA`
3. `Minimized DFA`
4. `Minimized DFA + Error state`

Example snippet (abridged):

```txt
NFA:
δ  | a | b | k | o
------------------
*S | C |   |   | A,D
...

DFA:
δ   | a  | b  | k  | o
----------------------
*M0 | M1 |    |    | M2
...

Minimized DFA + Error state:
δ   | a   | b   | k   | o
-------------------------
*M0 | ERR | ERR | ERR | M1
...
ERR | ERR | ERR | ERR | ERR
```

## Notes on Automata Properties

- The final `ERR` state is intentionally a sink state.
- In the final complete DFA, all transitions are defined for all symbols.
- State labels in DFA tables are compact aliases (`M0`, `M1`, ...) with a legend.

## Project Files

- Main implementation: [Main.hs](/home/arthur1/Projects/minimal-dfa-generator/minimal-dfa-generator/Main.hs)
- Example input: [input-file.txt](/home/arthur1/Projects/minimal-dfa-generator/minimal-dfa-generator/input-file.txt)

## Scientific Paper

https://docs.google.com/document/d/126fP6WipRx8Xs2B8v6HginA7noqyFThm/edit?usp=sharing&ouid=106734834979353198765&rtpof=true&sd=true

## Authors

- Arthur Antonio Bertoglio
- Davi Augusto Marangoni Mocva

## Academic Context

Computer Science  
Universidade Federal da Fronteira Sul (UFFS)  
Chapecó Campus, Brazil

## AI Disclosure

Parts of this project (including code and documentation refinements) were developed with assistance from AI tooling.

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE).