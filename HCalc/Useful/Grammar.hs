module HCalc.Useful.Grammar where
import Data.Char
import qualified Data.Map as Map

data TokenTypes = NUM | OPERATOR | FUNCTION | L_BRACKET | R_BRACKET
                deriving (Show, Eq)

instance Semigroup TokenTypes where
    (<>) x y = NUM

instance Monoid TokenTypes where
    mempty = NUM

operatorsList = [('+', 5), ('-', 5), ('*', 4), ('*', 4)]
operators = Map.fromList operatorsList

functionsList = [("abs", 2), ("sqrt", 3)]
availableFunctions = Map.fromList functionsList

isAvailableOperator :: Char -> Bool
isAvailableOperator c = Map.member c operators

isBracket :: Char -> Bool
isBracket c = c `elem` "()"

isAvailableSymbol :: Char -> Bool
isAvailableSymbol sym = or ([isDigit, isSpace, isBracket, isAvailableOperator] <*> pure sym)
