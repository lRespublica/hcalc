module HCalc.Useful.Grammar where
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import HCalc.Utils.List

data TokenTypes = NUM | OPERATOR | FUNCTION | L_BRACKET | R_BRACKET | COMMA
                deriving (Show, Eq)

instance Semigroup TokenTypes where
    (<>) x y = NUM

instance Monoid TokenTypes where
    mempty = NUM

usePlus :: Double -> Double -> Double
usePlus a b = a + b
useMinus :: Double -> Double -> Double
useMinus a b = a - b
useProd :: Double -> Double -> Double
useProd a b = a * b
useDiv :: Double -> Double -> Double
useDiv a b = a / b

operatorsList = [("+", (4, usePlus)), ("-", (4, useMinus)), ("*", (5, useProd)), ("/", (5, useDiv))]
availableOperators = Map.fromList operatorsList

compareOperators op1 op2 = case getPriorities of
                                Just [pr1, pr2] -> Just (compare pr1 pr2)
                                Nothing -> Nothing
    where
    getPriorities = fmap fst <$> sequenceA (Map.lookup <$> [op1, op2] <*> pure availableOperators)

useOperator :: String -> Double -> Double -> Maybe Double
useOperator op varX varY | isNothing operation  = Nothing
                         | otherwise = operation <*> pure varX <*> pure varY
    where
    operation = snd <$> Map.lookup op availableOperators

hasHigherPriority op1 op2 = comparision == Just LT || comparision == Just EQ
    where
    comparision = compareOperators op1 op2

functionsList = [("abs", 7), ("sqrt", 6)]
availableFunctions = Map.fromList functionsList

isAvailableOperator :: String -> Bool
isAvailableOperator c = Map.member c availableOperators

isAvailableCharOperator :: Char -> Bool
isAvailableCharOperator c = Map.member [c] availableOperators

isBracket :: Char -> Bool
isBracket c = c `elem` "()"

isAvailableSymbol :: Char -> Bool
isAvailableSymbol sym = or ([isDigit, isSpace, isBracket, isAvailableCharOperator] <*> pure sym)
