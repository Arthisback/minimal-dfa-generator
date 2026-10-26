# minimal-dfa-generator
# Minimal DFA Generator

A Haskell project for generating minimal Deterministic Finite Automata (DFAs) from token specifications and Regular Grammars (RGs).

## Overview

This project aims to develop a tool that processes input files containing token specifications or regular grammars and generates a minimal Deterministic Finite Automaton (DFA).

The generated automaton is intended to include an explicit error state and to be free of unreachable and dead states, without relying on state equivalence classes for minimization.

## Objectives

- Parse input files containing token specifications or Regular Grammars (RGs).
- Generate Deterministic Finite Automata (DFAs).
- Produce minimal automata without equivalence-class-based minimization.
- Handle invalid transitions through an explicit error state.
- Ensure that the generated automaton contains no unreachable or dead states.
- Validate the generated automata through testing.

## Planned Features

- *Input Processing:* Read and interpret supported input file formats.
- *Automaton Generation:* Construct a DFA from the provided input.
- *Error-State Handling:* Represent invalid transitions using an explicit error state.
- *State Validation:* Identify and handle unreachable and dead states.
- *Automaton Minimality:* Generate a minimal DFA without applying state equivalence classes.

## Technology Stack

- *Language:* Haskell
- 

## Project Structure

The project structure will be defined as the implementation evolves.

## Getting Started

### Prerequisites

Install the Glasgow Haskell Compiler (GHC).

### Installation

Clone the repository:

bash
git clone <git@github.com:Arthisback/minimal-dfa-generator.git>
cd minimal-dfa-generator


### Build and Run

Build and execution instructions will be added once the project setup is complete.

## Testing

Tests will be developed to verify input parsing, DFA generation, error-state handling, state reachability, and automaton minimality.

## Authors

Add the project contributors here.

## Academic Context

Computer Science  
Universidade Federal da Fronteira Sul (UFFS)  
Chapecó Campus, Brazil

## License

his project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.