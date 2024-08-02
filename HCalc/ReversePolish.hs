module HCalc.ReversePolish where

import HCalc.Useful.Parsers
import HCalc.Useful.Grammar
import HCalc.Utils.List
import Control.Applicative
import Data.Maybe

getAllTokens [] = Just []
getAllTokens str = (:) <$> newToken <*> getAllTokens (fromMaybe [] nextStr)
    where
    parseResult = sequenceA <$> parseToken str
    newToken = snd <$> parseResult
    nextStr = fst <$> parseResult

toReversePolish :: [(TokenTypes, [Char])] -> [(TokenTypes, [Char])]
toReversePolish [] = []
toReversePolish str = helper [] str
    where
    helper :: [(TokenTypes, [Char])] -> [(TokenTypes, [Char])] -> [(TokenTypes, [Char])]
    helper stack [] = stack
    helper stack (curToken:remainingStr) | tokenType == NUM       = curToken : helper stack remainingStr
                                         | tokenType == OPERATOR  = let splittedOperators = splitWhile (hasHigherPriority token . snd) stack
                                                                        pulled = fst splittedOperators
                                                                        remainingStack = snd splittedOperators
                                                                        in pulled ++ helper (curToken : remainingStack) remainingStr
                                         | tokenType == L_BRACKET = helper (curToken:stack) remainingStr
                                         | tokenType == R_BRACKET = let splitted = splitWhile (\x -> fst x /= L_BRACKET) stack
                                                                        pulled = fst splitted
                                                                        remainingStack = drop 1 . snd $ splitted
                                                                        in pulled ++ helper remainingStack remainingStr

        where
        (tokenType, token) = curToken

reduceReversePolish :: [(TokenTypes, [Char])] -> Maybe Double
reduceReversePolish = helper (Just [])
    where
    executeOperator :: [Char] -> [Double] -> Maybe Double
    executeOperator op operands = foldr1'' (useOperator op) (Just <$> take 2 operands)


    helper :: Maybe [Double] -> [(TokenTypes, [Char])] -> Maybe Double
    helper (Just [x]) [] = Just x
    helper _ [] = Nothing
    helper (Just operands) (x:xs) = case x of
                                (NUM, val) -> helper (Just (readDouble val : operands)) xs
                                (OPERATOR, val) -> if length' operands >=2
                                                        then helper ((:) <$> executeOperator val operands <*> Just(drop 2 operands)) xs
                                                        else helper Nothing xs
    helper Nothing _ = Nothing
