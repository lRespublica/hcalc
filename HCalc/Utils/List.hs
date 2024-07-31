module HCalc.Utils.List where

import Data.List

splitWhile cmp xs = (firstPart, xs \\ firstPart)
    where firstPart = takeWhile cmp xs
