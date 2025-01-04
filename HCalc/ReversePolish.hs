module HCalc.ReversePolish where

import HCalc.Useful.Parsers
import HCalc.Useful.Grammar
import HCalc.Utils.HFunctions
import HCalc.Utils.List
import HCalc.Utils.Token

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Either

type InputToken = Token
type OutputToken = Token
type TokenStack = [Token]
type OutputQuery = [Token]

type ErrorString = String

toReversePolish :: [Token] -> Either ErrorString [Token]
toReversePolish [] = Right []
toReversePolish list = helper list []
    where
    helper :: [InputToken] -> TokenStack -> Either ErrorString OutputQuery
    helper (x:xs) stack = do
                          (newStack, curOutput) <- sorter x stack
                          if x == END
                            then return curOutput
                            else do
                                 nextOutput <- helper xs newStack
                                 return (curOutput ++ nextOutput)

    -- Function responsible for processing one token according to the shunting yard algorithm
    sorter :: InputToken -> TokenStack -> Either ErrorString (TokenStack, OutputQuery)
    sorter a@(NUM _) stack = return (stack, [a])

    sorter fToken@(FUNCTION f) stack | fType == PREFIX = return (fToken : stack, [])
                                     | otherwise = return $ splitByPriority stack
        where
        fType = getType f

        splitByPriority :: TokenStack -> (TokenStack, OutputQuery)
        splitByPriority gs'@(FUNCTION g : gs) | getType g /= INFIX = (fToken:gs', [])
                                              | hasHigherPriority f g = (fToken:gs', [])
                                              | otherwise = (FUNCTION g:) <$> splitByPriority gs
        splitByPriority stack = (fToken:stack, [])

    sorter COMMA [] = Left "The expression is missing an opening parenthesis."
    sorter COMMA stack@(L_BRACKET : xs) = Right (stack, [])
    sorter COMMA (x:xs) = fmap (x:) <$> sorter COMMA xs

    sorter L_BRACKET stack = return (L_BRACKET : stack, [])

    sorter R_BRACKET [] = Left "The expression is missing an opening parenthesis."
    sorter R_BRACKET (L_BRACKET : FUNCTION f : stack) | getType f == PREFIX = return (stack, [FUNCTION f])
                                                      | otherwise = return (FUNCTION f : stack, [])
    sorter R_BRACKET (L_BRACKET : stack) = return (stack, [])
    sorter R_BRACKET (x:xs) = fmap (x:) <$> sorter R_BRACKET xs

    sorter END stack | L_BRACKET `elem` stack = Left "The expression is missing an closing parenthesis."
                     | otherwise = return ([], stack)

{-
reduceReversePolish :: [(Tokens, [Char])] -> Maybe Double
reduceReversePolish = helper (Just [])
    where
    executeOperator :: [Char] -> [Double] -> Maybe Double
    executeOperator op operands = foldr1'' (useOperator op) (Just <$> take 2 operands)


    helper :: Maybe [Double] -> [(Tokens, [Char])] -> Maybe Double
    helper (Just [x]) [] = Just x
    helper _ [] = Nothing
    helper (Just operands) (x:xs) = case x of
                                (NUM, val) -> helper (Just (readDouble val : operands)) xs
                                (OPERATOR, val) -> if length' operands >=2
                                                        then helper ((:) <$> executeOperator val operands <*> Just(drop 2 operands)) xs
                                                        else helper Nothing xs
    helper Nothing _ = Nothing
  -}
