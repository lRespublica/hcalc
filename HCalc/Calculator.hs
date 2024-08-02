module HCalc.Calculator where

import HCalc.Parse
import HCalc.ReversePolish
import HCalc.Useful.Checks

calculate :: String -> Either String Double
calculate str = case validationCheck clearStr of
                    Right True -> let tokens = getAllTokens clearStr
                                      reverse = toReversePolish <$> tokens
                                      in case reverse of
                                         Nothing -> Left "Invalid expression"
                                         Just val -> let result = reduceReversePolish val
                                                     in case result of
                                                        Nothing -> Left "Invalid expression"
                                                        Just value -> Right value
                    Right False -> Left "Checks have not passed"
                    Left message -> Left message
    where
    clearStr = clearFromSpaces str
