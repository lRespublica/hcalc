module HCalc.Useful.Parsers where

import HCalc.Parser
import HCalc.Useful.Char
import Data.Char
import Control.Applicative

anySymb = satisfy (const True)
digit = satisfy isDigit
space = satisfy isSpace
symbol = satisfy isAvailableMathSymbol
bracket = satisfy isBracket

num = some digit

allBrackets = (:) <$> bracket <*> allBrackets <|> anySymb *> allBrackets <|> pure []
