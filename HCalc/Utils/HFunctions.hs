module HCalc.Utils.HFunctions where

import HCalc.Utils.HNumbers

data HFunType = INFIX | PREFIX deriving (Eq, Show, Ord)

class (Eq a, Show a) => HFunc a where
    getStrs :: a -> [String]
    getPriority :: a -> Int
    getType :: a -> HFunType
    execFunc :: a -> [HNum] -> Maybe HNum

data Operators = PLUS | MINUS | MUL | DIV deriving (Eq, Show, Enum, Bounded)

instance HFunc Operators where
    getStrs PLUS = ["+"]
    getStrs MINUS = ["-"]
    getStrs MUL = ["*"]
    getStrs DIV = ["/"]

    getPriority PLUS = 4
    getPriority MINUS = 4
    getPriority MUL = 5
    getPriority DIV = 5

    getType _ = INFIX

    execFunc PLUS [a, b] = Just $ a + b
    execFunc MINUS [a, b] = Just $ a - b
    execFunc MUL [a, b] = Just $ a * b
    execFunc DIV [a, b] = Just $ a / b
    execFunc _ _ = Nothing
