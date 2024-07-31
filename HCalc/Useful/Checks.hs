module HCalc.Useful.Checks where

import HCalc.Useful.Grammar

clearFromSpaces :: String -> String
clearFromSpaces = filter (/=' ')

checkBracketsStr :: Maybe [Char] -> Either String Bool
checkBracketsStr Nothing    = Left "Parser error"
checkBracketsStr (Just str) = helper 0 0 str
    where
    helper lCount rCount str = case str of
                                        [] ->
                                            if correctFinal then Right True
                                            else Left "Missmatched brackets"
                                        (x:xs) ->
                                            if correctCount then case x of
                                                '(' -> helper (lCount+1) rCount xs
                                                ')' -> helper lCount (rCount+1) xs
                                                _   -> Left ("Unknown symbol '" ++ x : "' in bracket structure")
                                            else Left "Missmatched brackets"
        where
        correctCount = lCount >= rCount
        correctFinal = lCount == rCount

onlyAvailableSymbols :: [Char] -> Either String Bool
onlyAvailableSymbols [] = Right True
onlyAvailableSymbols (x:xs) | isAvailableSymbol x = onlyAvailableSymbols xs
                            | otherwise           = Left ("Bad symbol '" ++ x : "'")
