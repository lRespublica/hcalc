module HCalc.Useful.Parsers where

import HCalc.Utils.Parser
import HCalc.Useful.Grammar
import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative

char x = satisfy (==x)

string :: String -> Parser Char String
string = foldr (\x y -> (:) <$> char x <*> y) (pure "")

anySymb = satisfy (const True)
digit = satisfy isDigit
space = satisfy isSpace
operator = satisfy isAvailableOperator
bracket = satisfy isBracket
lBracket = string "("
rBracket = string ")"
comma = string ","

num = some digit

allBrackets = (:) <$> bracket <*> allBrackets <|> anySymb *> allBrackets <|> pure []

allFunctions = [ string (fst x) | x <- functionsList]
allOperators = [ string (fst x) | x <- operatorsList]

runParsersList parsers val = fromMaybe Nothing findFunc
    where
    parsersResult = runParser <$> parsers <*> pure val
    findFunc = find (/= Nothing) parsersResult

simpleParsers = [(NUM, num), (L_BRACKET, lBracket), (R_BRACKET, rBracket), (COMMA, comma)]
complexParsers = [(OPERATOR, allOperators), (FUNCTION, allFunctions)]

runSimpleParsers :: [(TokenTypes, [Char] -> Maybe ([Char], [Char]))]
runSimpleParsers = fmap runParser <$> simpleParsers
runComplexParsers = fmap runParsersList <$> complexParsers
runAllParsers = runSimpleParsers ++ runComplexParsers

parseToken str = fromMaybe Nothing findToken
    where
    parsersResult = sequenceA <$> (sequenceA <$> runAllParsers <*> pure str)
    findToken = find (/= Nothing) parsersResult
