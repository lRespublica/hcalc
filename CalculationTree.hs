import GHC.Real (reduce)
data BinaryOp = MUL | DIV | SUM | DIFF
                deriving (Show, Eq, Ord)

getFunc :: (Num a, Integral a) => BinaryOp -> (a -> a -> a)
getFunc = helper
    where
    helper MUL  = (*)
    helper DIV  = div
    helper SUM  = (+)
    helper DIFF = (-)

data CalcTree a = (Num a, Integral a) => Value !a |
                  (Num a, Integral a) => CalcNode !(CalcTree a) !BinaryOp !(CalcTree a)


reduceTree :: CalcTree a -> a
reduceTree (Value val) = val
reduceTree (CalcNode left op right) = getFunc op (reduceTree left) (reduceTree right)
