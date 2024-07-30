module HCalc.Useful.Char where
import Data.Char

availableMathSymbols = "+-*/"

isAvailableMathSymbol :: Char -> Bool
isAvailableMathSymbol c = c `elem` availableMathSymbols

isBracket :: Char -> Bool
isBracket c = c `elem` "()"

isAvailableSymbol :: Char -> Bool
isAvailableSymbol sym = or ([isDigit, isSpace, isBracket, isAvailableMathSymbol] <*> pure sym)
