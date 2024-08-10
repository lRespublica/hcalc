{-# LANGUAGE GADTs, StandaloneDeriving #-}
module HCalc.Utils.Token where

import HCalc.Utils.HNumbers
import HCalc.Utils.HFunctions

data Token where
    NUM :: HNum -> Token
    FUNCTION :: HFunc f => f -> Token
    L_BRACKET :: Token
    R_BRACKET :: Token
    COMMA :: Token
    END :: Token

getNum :: Token -> Maybe HNum
getNum (NUM a) = Just a
getNum _ = Nothing

instance Eq Token where
    (NUM a) == (NUM b) = a == b
    (FUNCTION a) == (FUNCTION b) = show a == show b
    L_BRACKET == L_BRACKET = True
    R_BRACKET == R_BRACKET = True
    COMMA == COMMA = True
    END == END = True

    _ == _ = False

deriving instance Show Token
