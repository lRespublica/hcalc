module HCalc.Utils.HFunctions where

import HCalc.Utils.HNumbers

import Data.Maybe

data HFunType = INFIX | PREFIX deriving (Eq, Show, Ord)

type NumStack = [HNum]

class (Eq a, Show a) => HFunc a where
    getStrs :: a -> [String]
    getPriority :: a -> Int
    getType :: a -> HFunType
    execFunc :: a -> NumStack -> Either String NumStack

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

    execFunc PLUS (a:b:xs) = Right ((b + a):xs)
    execFunc MINUS (a:b:xs) = Right ((b - a):xs)
    execFunc MUL (a:b:xs) = Right ((b * a):xs)
    execFunc DIV (a:b:xs) = Right ((b / a):xs)
    execFunc _ _ = Left "Not enough operands"
