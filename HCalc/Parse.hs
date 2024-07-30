module HCalc.Parse where

import HCalc.Useful.Parsers
import HCalc.Useful.Strings
import HCalc.Utils.Parser

checkBrackets str = snd $ checkBracketsStr <$> sequenceA (runParser allBrackets str)

useChecks :: [String -> Either String Bool] -> String -> Either String Bool
useChecks checks str = and <$> sequenceA (checks <*> pure str)

validationCheck :: String -> Either String Bool
validationCheck = useChecks [checkBrackets, onlyAvailableSymbols]
