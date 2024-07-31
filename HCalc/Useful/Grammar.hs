module HCalc.Useful.Grammar where
import Data.Char
import qualified Data.Map as Map

availableMathSymbols = "+-*/"

operations = Map.fromList [('+', 5), ('-', 5), ('*', 4), ('*', 4)]

functionsList = [("abs", 2), ("sqrt", 3)]
availableFunctions = Map.fromList functionsList

isAvailableMathSymbol :: Char -> Bool
isAvailableMathSymbol c = c `elem` availableMathSymbols

isBracket :: Char -> Bool
isBracket c = c `elem` "()"

isAvailableSymbol :: Char -> Bool
isAvailableSymbol sym = or ([isDigit, isSpace, isBracket, isAvailableMathSymbol] <*> pure sym)
