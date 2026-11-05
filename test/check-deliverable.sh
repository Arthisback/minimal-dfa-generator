#!/usr/bin/env bash
# Confere se o arquivo único (MinimalDfaGenerator.hs) se comporta como a versão em
# módulos. Rode na raiz do repositório com: bash test/check-deliverable.sh
set -euo pipefail

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# A mesma suíte de testes, importando as funções do arquivo único.
sed 's/^module Main (main) where$/module Deliverable where/' MinimalDfaGenerator.hs > "$work/Deliverable.hs"
sed -e '/^import Automata\./d' \
    -e 's/^module Main (main) where$/&\n\nimport Deliverable (Dfa (..), DfaState, Nfa (..), StateName, Symbol, TransitionMap, addErrorState, buildDfa, buildNfa, errorState, parseInput, removeDeadStates, removeUnreachableStates)/' \
    test/Spec.hs > "$work/Spec.hs"
(cd "$work" && runghc Spec.hs)

# A mesma saída, byte a byte, nos arquivos de exemplo.
for input in input-file.txt examples/*.txt; do
    diff <(runghc Main.hs "$input") <(runghc MinimalDfaGenerator.hs "$input")
    echo "Same output: $input"
done
