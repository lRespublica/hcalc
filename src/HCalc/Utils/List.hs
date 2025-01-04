module HCalc.Utils.List where

import Data.List

splitWhile cmp xs = (firstPart, xs \\ firstPart)
    where firstPart = takeWhile cmp xs

length' :: [a] -> Int
length' = length

readDouble :: String -> Double
readDouble = read

foldr1'' :: Foldable t => (a -> a -> a) -> t a -> a
foldr1'' = foldr1
