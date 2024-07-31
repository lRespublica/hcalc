module HCalc.Useful.Parsers where

import HCalc.Utils.Parser
import HCalc.Useful.Grammar
import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative

anySymb = satisfy (const True)
digit = satisfy isDigit
char x = satisfy (==x)
space = satisfy isSpace
symbol = satisfy isAvailableMathSymbol
bracket = satisfy isBracket
lBracket = char '('
rBracket = char ')'

string :: String -> Parser Char String
string = foldr (\x y -> (:) <$> char x <*> y) (pure "")

num = some digit

allBrackets = (:) <$> bracket <*> allBrackets <|> anySymb *> allBrackets <|> pure []

allFunctions = [ string (fst x) | x <- functionsList]

parseFunc :: String -> Maybe (String, String)
parseFunc str = fromMaybe Nothing findFunc
    where
    parsersResult = runParser <$> allFunctions <*> pure str
    findFunc = find (/= Nothing) parsersResult
