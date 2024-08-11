module HCalc.Useful.Grammar where

import Data.Char
import Data.Maybe
import qualified Data.Map as Map

import Control.Monad

import HCalc.Utils.List
import HCalc.Utils.HNumbers
import HCalc.Utils.HFunctions
import HCalc.Utils.Token

allOperators :: [Operators]
allOperators = [minBound .. maxBound]

operatorStringsList = toStringOperatorPair =<< allOperators
    where toStringOperatorPair op = (, op) <$> getStrs op

availableOperators :: Map.Map String Operators
availableOperators = Map.fromList operatorStringsList

compareFunctions :: (HFunc f, HFunc g) => f -> g -> Ordering
compareFunctions a b = compare (getPriority a) (getPriority b)

useFunction :: HFunc f => f -> [Token] -> Maybe Token
useFunction f tokens = do
                       arr <- mapM getNum tokens
                       NUM <$> execFunc f arr

hasHigherPriority :: (HFunc f, HFunc g) => f -> g -> Bool
hasHigherPriority fun1 fun2 = comparision == GT
    where
    comparision = compareFunctions fun1 fun2

isAvailableOperator :: String -> Bool
isAvailableOperator str = Map.member str availableOperators

isBracket :: Char -> Bool
isBracket c = c `elem` "()"

isAvailableSymbol :: Char -> Bool
isAvailableSymbol sym = or ([isDigit, isSpace, isBracket, isSymbol, (=='.'), isAlpha] <*> pure sym)
