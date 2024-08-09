module HCalc.Utils.HNumbers where
import GHC.Real (Ratio( (:%) ))
import GHC.Float (Double())

data HNum = HInt !Integer | HDouble !Double
data HFunType = INFIX | POSTFIX deriving (Eq, Show, Ord)

toNormalForm :: HNum -> HNum
toNormalForm (HInt a) = HInt a

toNormalForm (HDouble a) | hasFractionalPart = HDouble a
                         | canBeFloored = HInt floored
                         | canBeCeiled = HInt ceiled
    where
    hasFractionalPart = not (canBeFloored || canBeCeiled)
    canBeFloored = abs (fromIntegral floored - a) < epsilon
    canBeCeiled = abs (fromIntegral ceiled - a) < epsilon

    floored = floor a
    ceiled = ceiling a
    epsilon = 10 ** (-10)

instance Show HNum where
    show (HInt a) = show a
    show (HDouble a) = show a

instance Eq HNum where
    (HInt a) == (HInt b) = a == b
    a@(HInt a') == b@(HDouble b') | bInt@(HInt _) <- toNormalForm b = a == bInt
                                  | otherwise = False
    a@(HDouble _) == b@(HInt _) = b == a
    HDouble a == HDouble b = a == b

instance Ord HNum where
    HInt a <= HInt b = a <= b
    HDouble a <= HDouble b = a <= b

    HInt a <= HDouble b = a <= floor b
    HDouble a <= HInt b = ceiling a <= b

instance Num HNum where
    HInt a + HInt b = HInt $ a + b
    HDouble a + HInt b = toNormalForm . HDouble $ a + fromIntegral b
    a@(HInt _) + b@(HDouble _) = b + a
    HDouble a + HDouble b = toNormalForm . HDouble $ a + b

    HInt a * HInt b = HInt $ a * b
    HDouble a * HInt b = toNormalForm . HDouble $ a * fromIntegral b
    a@(HInt _) * b@(HDouble _) = b * a
    HDouble a * HDouble b = toNormalForm . HDouble $ a * b

    abs (HInt a) = HInt $ abs a
    abs (HDouble a) = HDouble $ abs a

    fromInteger = HInt

    signum (HInt a) = HInt $ signum a
    signum (HDouble a) = HInt $ ceiling . signum $ a

    negate (HInt a) = HInt $ negate a
    negate (HDouble a) = HDouble $ negate a

instance Real HNum where
    toRational (HInt a) = toRational a
    toRational (HDouble a) = toRational a

instance Fractional HNum where
    fromRational (x :% y) | (x `mod` y) == 0 = HInt (x `div` y)
                          | otherwise = foldr ((/) . fromIntegral) 1 [x, y]

    (HInt a) / (HInt b) | a `mod` b == 0 = HInt $ a `div` b
                        | otherwise = HDouble $ foldr ((/) . fromIntegral) 1 [a, b]

    (HDouble a) / (HInt b) = toNormalForm . HDouble $ a / fromIntegral b
    (HInt a) / (HDouble b) = toNormalForm . HDouble $ fromIntegral a / b

    (HDouble a) / (HDouble b) = toNormalForm . HDouble $ a / b
