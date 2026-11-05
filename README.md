# Minimal DFA Generator

Haskell project that reads tokens and regular grammars (RGs), builds a single NFA, determinizes it, removes unreachable and dead states, and finally adds an explicit error state.

The result is the automaton required by the project statement: a DFA free of unreachable and dead states, with an error state, minimized **without equivalence classes between states**.

## Features

- Parses an input file with:
  - token lines
  - grammar lines in the form `<S> ::= a<A> | ε`
- Builds **one combined NFA**:
  - shared initial state `S`
  - token paths with fresh states
  - each grammar with its own states
- Builds the DFA using subset construction
- Minimizes the DFA by removing unreachable and dead states, without merging states
- Adds an explicit `ERR` state after minimization:
  - all missing transitions point to `ERR`
  - `ERR` loops to itself for all symbols
  - `ERR` is a final state, as the statement requires
- Prints aligned transition tables for:
  - NFA
  - DFA
  - Minimized DFA
  - Minimized DFA + Error state

## Why states are not merged

Each final state identifies what was recognized: in the example of the statement, `G` recognizes `entao` and `L` recognizes `senao`. Merging equivalent states would join them, and the automaton could no longer tell the two tokens apart.

This is why the minimization only removes states. Algorithms that do not compute equivalence classes explicitly, such as Brzozowski's, are not an alternative: they produce the same merged automaton.

## Input Format

Each non-empty line is either:

1. **Grammar production** (line starts with a non-terminal, such as `<A>`)
2. **Token** (any other line, including operators such as `<`, `<=` and `<>`)

Tokens and productions can be interleaved. Blank lines are ignored.

### Grammar syntax

```
<NonTerminal> ::= a<OtherNonTerminal> | b | ε
```

Supported right-hand alternatives:

- `a<NT>` terminal + destination non-terminal
- `a` terminal-only transition
- `ε` epsilon (marks current non-terminal state as final)

A file can hold several grammars. **Each `<S>` line starts a new grammar**, so all the alternatives of `<S>` go on a single line. Non-terminals belong to their grammar: two grammars can both use `<A>` without sharing states.

The `|` character cannot be a terminal of a grammar, but it can be a token.

## Example Input

```txt
se
entao
senao

<S> ::= a<A> | e<A> | i<A> | o<A> | u<A>
<A> ::= a<A> | e<A> | i<A> | o<A> | u<A> | ε
```

## Running

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- `runghc` available in PATH

### Execute

[MinimalDfaGenerator.hs](MinimalDfaGenerator.hs) is the deliverable: all the code in a single, commented file, with no other module needed.

```bash
runghc MinimalDfaGenerator.hs input-file.txt
```

The modular version gives the same results. Run it from the repository root, so that the `Automata` modules are found:

```bash
runghc Main.hs input-file.txt
```

The [examples](examples) folder has more inputs:

| File | Shows |
| --- | --- |
| [statement-example.txt](examples/statement-example.txt) | The example of the project statement |
| [dead-and-unreachable.txt](examples/dead-and-unreachable.txt) | Dead states and an unreachable non-terminal |
| [multiple-grammars.txt](examples/multiple-grammars.txt) | Operators and two grammars that share the name `<A>` |

### Test

```bash
runghc test/Spec.hs
```

The suite covers the example of the statement, malformed inputs and edge cases. It also generates pseudo-random inputs and compares every automaton of the pipeline against the grammars themselves, on all words of up to 5 symbols.

To run the same suite on the single file and compare its output with the modular version:

```bash
bash test/check-deliverable.sh
```

## Expected Output (Example)

The program prints 4 transition tables in sequence:

1. `NFA`
2. `DFA`
3. `Minimized DFA`
4. `Minimized DFA + Error state`

Final states are marked with `*`, and a DFA state is named after the NFA states it stands for. A state keeps its name in every table.

Example snippet for [statement-example.txt](examples/statement-example.txt) (abridged):

```txt
NFA:
δ  | s   | e   | n | t | a | o | i | u
--------------------------------------
S  | A,H | C,M |   |   | M | M | M | M
...

DFA:
Unreachable NFA states left out: none

δ      | s     | e     | n   | t   | a   | o   | i   | u
----------------------------------------------------------
{S}    | {A,H} | {C,M} |     |     | {M} | {M} | {M} | {M}
...

Minimized DFA + Error state:
δ      | s     | e     | n   | t   | a   | o   | i   | u
----------------------------------------------------------
{S}    | {A,H} | {C,M} | ERR | ERR | {M} | {M} | {M} | {M}
...
*ERR   | ERR   | ERR   | ERR | ERR | ERR | ERR | ERR | ERR
```

## Notes on Automata Properties

- Subset construction starts at the initial state, so NFA states that cannot be reached never enter the DFA. The output lists them. The unreachable-state pass stays as an explicit step and is tested on its own.
- When the language is empty, the initial state stays even though it is dead, because a DFA needs an initial state.
- `ERR` is final because the statement asks for it. Reaching it does not accept the sentence: it is where a recognizer stops and reports the error. Being final also keeps it from being a dead state.
- `ERR` is not added when the DFA has no unmapped cell, because it would be unreachable.
- A symbol used only by removed states stays in the alphabet, and its column leads to `ERR`.
- A production that goes back to `<S>` uses a private copy of the state. An edge into the shared initial state would let a sentence continue into the other tokens and grammars.

## Project Structure

[MinimalDfaGenerator.hs](MinimalDfaGenerator.hs) holds the whole program, with one numbered section per stage. The same code is also split into modules, each matching a section of the project statement:

| Module | Responsibility |
| --- | --- |
| [Main.hs](Main.hs) | Reads the file and prints each stage |
| [Automata/Input.hs](Automata/Input.hs) | Parses tokens and BNF productions |
| [Automata/Nfa.hs](Automata/Nfa.hs) | Loads everything into a single NFA |
| [Automata/Determinization.hs](Automata/Determinization.hs) | Subset construction |
| [Automata/Minimization.hs](Automata/Minimization.hs) | Removes unreachable and dead states |
| [Automata/ErrorState.hs](Automata/ErrorState.hs) | Adds the error state |
| [Automata/Render.hs](Automata/Render.hs) | Prints the transition tables |
| [Automata/Types.hs](Automata/Types.hs) | Shared types |

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
