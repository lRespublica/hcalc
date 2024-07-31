module HCalc.Useful.Grammar where
import Data.Char
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import HCalc.Utils.List

data TokenTypes = NUM | OPERATOR | FUNCTION | L_BRACKET | R_BRACKET | COMMA
                deriving (Show, Eq)

instance Semigroup TokenTypes where
    (<>) x y = NUM

instance Monoid TokenTypes where
    mempty = NUM

operatorsList = [("+", 4), ("-", 4), ("*", 5), ("*", 5)]
availableOperators = Map.fromList operatorsList

compareOperators op1 op2 = case getPriorities of
                                Just [pr1, pr2] -> Just (compare pr1 pr2)
                                Nothing -> Nothing
    where
    getPriorities = sequenceA (Map.lookup <$> [op1, op2] <*> pure availableOperators)

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
