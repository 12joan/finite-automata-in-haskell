module RegularExpression (RegularExpression (EmptyWord, EmptySet, Symbol, Disjunct, Concat, Kleene), parseRegularExpression, regexToNFA, acceptsRegex) where

import NFA
import NFABuilder

import Text.Parsec (parse, (<|>), eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (between)
import Text.Parsec.Char (anyChar, char)
import qualified Text.Parsec.Expr as E

data RegularExpression a = EmptyWord
                         | EmptySet
                         | Symbol a
                         | Disjunct (RegularExpression a) (RegularExpression a)
                         | Concat (RegularExpression a) (RegularExpression a)
                         | Kleene (RegularExpression a)
                         deriving (Eq, Show)

acceptsRegex :: Eq a => RegularExpression a -> [a] -> Bool
acceptsRegex regex = acceptsNFA (regexToNFA regex)

regexToNFA :: Eq a => RegularExpression a -> NFA StateId a
regexToNFA = buildNFA . regexToNFABuilder

regexToNFABuilder :: Eq a => RegularExpression a -> NFABuilder a
regexToNFABuilder EmptyWord = emptyWordNFA
regexToNFABuilder EmptySet = emptySetNFA
regexToNFABuilder (Symbol x) = acceptSymbolNFA x
regexToNFABuilder (Disjunct x y) = do
  x' <- regexToNFABuilder x
  y' <- regexToNFABuilder y
  disjunctNFAs x' y'
regexToNFABuilder (Concat x y) = do
  x' <- regexToNFABuilder x
  y' <- regexToNFABuilder y
  concatNFAs x' y'
regexToNFABuilder (Kleene x) = do
  x' <- regexToNFABuilder x
  kleeneNFA x'

parseRegularExpression :: String -> Maybe (RegularExpression Char)
parseRegularExpression input =
  case (parse (regexParser <* eof) "" input) of
    (Right x) -> Just x
    (Left _)  -> Nothing

regexParser :: Parser (RegularExpression Char)
regexParser = E.buildExpressionParser regexTable parseRegexTerm

regexTable = [
  [E.Postfix (Kleene   <$ char '*')],
  [E.Infix   (Concat   <$ char '.') E.AssocLeft],
  [E.Infix   (Disjunct <$ char '+') E.AssocLeft]
             ]

parseRegexTerm :: Parser (RegularExpression Char)
parseRegexTerm = parseParens <|> parseEmptyWord <|> parseEmptySet <|> parseSymbol

parseParens :: Parser (RegularExpression Char)
parseParens = between (char '(') (char ')') regexParser

parseEmptyWord :: Parser (RegularExpression Char)
parseEmptyWord = EmptyWord <$ (char 'ε')

parseEmptySet :: Parser (RegularExpression Char)
parseEmptySet = EmptySet <$ (char '∅')

parseSymbol :: Parser (RegularExpression Char)
parseSymbol = Symbol <$> anyChar
