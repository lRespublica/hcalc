module HCalc.Useful.Char where

availableMathSymbols = "+-*/"

isAvailableMathSymbol :: Char -> Bool
isAvailableMathSymbol c = c `elem` availableMathSymbols

isBracket :: Char -> Bool
isBracket c = c `elem` "()"
