module HCalc.Useful.Parsers where

import HCalc.Utils.Parser
import HCalc.Useful.Grammar
import Data.Char
import Control.Applicative

anySymb = satisfy (const True)
digit = satisfy isDigit
space = satisfy isSpace
symbol = satisfy isAvailableMathSymbol
bracket = satisfy isBracket
lBracket = satisfy (=='(')
rBracket = satisfy (==')')

num = some digit

allBrackets = (:) <$> bracket <*> allBrackets <|> anySymb *> allBrackets <|> pure []
