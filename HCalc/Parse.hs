module HCalc.Parse where

import HCalc.Useful.Parsers
import HCalc.Useful.Strings
import HCalc.Utils.Parser

checkBrackets str = snd $ checkBracketsStr <$> sequenceA (runParser allBrackets str)

validationCheck :: String -> Either String Bool
validationCheck str = and <$> sequenceA ([checkBrackets] <*> pure str)
