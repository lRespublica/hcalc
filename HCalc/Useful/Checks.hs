module HCalc.Useful.Checks where

import HCalc.Useful.Grammar

clearFromSpaces :: String -> String
clearFromSpaces = filter (/=' ')
