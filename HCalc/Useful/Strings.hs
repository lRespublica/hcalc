module HCalc.Useful.Strings where

checkBrackets :: [Char] -> Either String Bool
checkBrackets = helper 0 0
    where
    helper lCount rCount str = case str of
                                        [] -> correctFinal
                                        ('(':xs) -> correctCount <*> helper (lCount+1) rCount xs
                                        (')':xs) -> correctCount <*> helper lCount (rCount+1) xs
                                        (x:_)    -> Left ("Unknown symbol '" ++ x : "' in bracket structure")
        where
        correctCount = (&&) <$> Right (lCount >= rCount)
        correctFinal = Right (lCount == rCount)
