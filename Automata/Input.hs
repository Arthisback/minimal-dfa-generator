-- | Lê o texto do arquivo de entrada, que traz as sentenças na forma de tokens
-- ou de gramáticas regulares em notação BNF.
--
-- Vários resultados aqui usam Either String: Left traz uma mensagem de erro e
-- Right traz o valor. Em um bloco do com Either, o primeiro Left interrompe tudo.
module Automata.Input
    ( Alt (..)
    , Production (..)
    , Input (..)
    , parseInput
    ) where

import Automata.Types (Symbol)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, partition)

-- | Uma alternativa do lado direito de uma produção. São as três formas
-- permitidas em uma gramática regular:
--
--   ε    -> Epsilon
--   a    -> TermOnly 'a'
--   a<B> -> TermToNonTerminal 'a' "B"
data Alt
    = Epsilon
    | TermOnly Symbol
    | TermToNonTerminal Symbol String

-- | Uma linha de gramática. @<A> ::= a<A> | ε@ vira o lado esquerdo pLhs = "A"
-- e as duas alternativas em pAlts.
data Production = Production
    { pLhs :: String
    , pAlts :: [Alt]
    }

-- | Tudo o que foi lido do arquivo. Cada gramática é uma lista de produções, e
-- inputGrammars é a lista de gramáticas.
data Input = Input
    { inputTokens :: [String]
    , inputGrammars :: [[Production]]
    }

-- | Separa as linhas em tokens e produções. Os dois tipos podem aparecer
-- misturados no arquivo, e linhas em branco são ignoradas.
--
-- O trim remove os espaços nas pontas de cada linha e também o \r que os
-- arquivos do Windows têm no fim das linhas. O mapM aplica parseProduction em
-- todas as linhas e falha se alguma delas falhar.
parseInput :: String -> Either String Input
parseInput contents = do
    let inputs = filter (not . null) (map trim (lines contents))
        (grammarLines, tokens) = partition isProductionLine inputs
    productions <- mapM parseProduction grammarLines
    Right Input {inputTokens = tokens, inputGrammars = groupGrammars productions}

-- | Uma linha é produção quando começa com um não-terminal, como @<S>@. Os
-- operadores @<@, @<=@ e @<>@ não têm um nome válido entre < e >, então continuam
-- sendo tokens. O enunciado cita operadores, por isso não basta olhar se a linha
-- começa com <.
--
-- O span separa o início da string formado por caracteres de nome. O padrão
-- (_ : _, '>' : _) exige ao menos um caractere de nome seguido de >.
isProductionLine :: String -> Bool
isProductionLine ('<' : rest) =
    case span isNameChar rest of
        (_ : _, '>' : _) -> True
        _ -> False
isProductionLine _ = False

-- | Caracteres permitidos no nome de um não-terminal.
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

-- | Divide as produções em gramáticas: cada novo @<S>@ abre uma nova gramática.
-- Isso permite que duas gramáticas usem o mesmo nome, como @<A>@, sem misturar
-- seus estados.
--
-- O break corta a lista na primeira produção de @<S>@. Depois, ele é usado de
-- novo para achar o próximo @<S>@, e tudo entre os dois forma uma gramática. As
-- linhas que vêm antes do primeiro @<S>@ também entram nessa gramática.
groupGrammars :: [Production] -> [[Production]]
groupGrammars [] = []
groupGrammars productions =
    case break isStartProduction productions of
        (beforeStart, []) -> [beforeStart]
        (beforeStart, start : afterStart) ->
            let (body, others) = break isStartProduction afterStart
             in (beforeStart ++ start : body) : groupGrammars others
  where
    isStartProduction production = pLhs production == "S"

-- | Remove os espaços do fim e do início. Como dropWhile só atua no começo da
-- string, ela é invertida para tratar o final.
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Divide uma string em um separador: "a|b" com '|' vira ["a", "b"].
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy sep xs =
    case break (== sep) xs of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitBy sep rest

-- | Interpreta uma linha de gramática, como @<A> ::= a<A> | ε@: corta no ::=,
-- lê o não-terminal da esquerda, divide a direita em | e lê cada alternativa.
parseProduction :: String -> Either String Production
parseProduction line =
    let (lhsPart, rhsPartRaw) = breakSubstring "::=" line
     in if null rhsPartRaw
            then Left ("Invalid grammar line (missing ::=): " ++ line)
            else do
                lhs <- parseNonTerminal (trim lhsPart)
                -- drop 3 tira o próprio ::= do início do lado direito.
                let rhsPart = trim (drop 3 rhsPartRaw)
                alts <- mapM parseAlt (map trim (splitBy '|' rhsPart))
                Right Production {pLhs = lhs, pAlts = alts}
  where
    -- Corta o texto em duas partes, no ponto onde o trecho procurado aparece. A
    -- segunda parte começa com o trecho e fica vazia quando ele não existe.
    breakSubstring needle haystack = search [] haystack
      where
        search _ [] = (haystack, [])
        -- O nome remaining@(x : xs) dá acesso à lista inteira e às suas partes.
        search acc remaining@(x : xs)
            | needle `isPrefixOf` remaining = (reverse acc, remaining)
            | otherwise = search (x : acc) xs

-- | Extrai o nome de um não-terminal: "<A>" vira "A". Para conferir o > do
-- final, a string é invertida. Tudo fora desse formato é erro.
parseNonTerminal :: String -> Either String String
parseNonTerminal text =
    case text of
        '<' : xs ->
            case reverse xs of
                '>' : revName ->
                    let name = reverse revName
                     in if null name || not (all isNameChar name)
                            then Left ("Invalid non-terminal: " ++ text)
                            else Right name
                _ -> Left ("Invalid non-terminal: " ++ text)
        _ -> Left ("Invalid non-terminal: " ++ text)

-- | Interpreta uma alternativa. O primeiro caractere é o terminal e o resto é
-- vazio ou um não-terminal. Com isso, ab<A> dá erro: gramática regular só aceita
-- um terminal por alternativa. O <$> aplica o construtor ao resultado de
-- parseNonTerminal, se ele não for um erro.
parseAlt :: String -> Either String Alt
parseAlt "ε" = Right Epsilon
parseAlt "" = Left "Empty alternative in grammar"
parseAlt (symbol : restChars) =
    case trim restChars of
        "" -> Right (TermOnly symbol)
        rest -> TermToNonTerminal symbol <$> parseNonTerminal rest
