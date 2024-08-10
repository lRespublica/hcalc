{-# LANGUAGE GADTs, StandaloneDeriving #-}
module HCalc.Utils.TokenType where

import HCalc.Utils.HNumbers
import HCalc.Utils.HFunctions

data TokenType where
    NUM :: HNum -> TokenType
    FUNCTION :: HFunc f => f -> TokenType
    L_BRACKET :: TokenType
    R_BRACKET :: TokenType
    COMMA :: TokenType
    END :: TokenType

getNum :: TokenType -> Maybe HNum
getNum (NUM a) = Just a
getNum _ = Nothing

instance Eq TokenType where
    (NUM a) == (NUM b) = a == b
    (FUNCTION a) == (FUNCTION b) = show a == show b
    L_BRACKET == L_BRACKET = True
    R_BRACKET == R_BRACKET = True
    COMMA == COMMA = True
    END == END = True

    _ == _ = False

deriving instance Show TokenType
