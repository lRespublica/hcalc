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
