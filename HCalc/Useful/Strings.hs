module HCalc.Useful.Strings where

checkBracketsStr :: Maybe [Char] -> Either String Bool
checkBracketsStr Nothing    = Left "Parser error"
checkBracketsStr (Just str) = helper 0 0 str
    where
    helper lCount rCount str = case str of
                                        [] -> correctFinal
                                        ('(':xs) -> correctCount <*> helper (lCount+1) rCount xs
                                        (')':xs) -> correctCount <*> helper lCount (rCount+1) xs
                                        (x:_)    -> Left ("Unknown symbol '" ++ x : "' in bracket structure")
        where
        correctCount = (&&) <$> Right (lCount >= rCount)
        correctFinal = Right (lCount == rCount)
