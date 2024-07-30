module HCalc.Parser where
import Control.Applicative (Alternative(..))

newtype Parser tok a = Parser { runParser :: [tok] -> Maybe ([tok],a) }

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
    f (c:cs) | pr c = Just (cs,c)
    f _ = Nothing

instance Functor (Parser tok) where
    fmap :: (a -> b) -> Parser tok a -> Parser tok b
    fmap g = Parser . (fmap . fmap . fmap) g . runParser

instance Applicative (Parser tok) where
    pure :: a -> Parser tok a
    pure x = Parser $ \s -> Just (s, x)

    (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
    Parser u <*> Parser v = Parser f where
        f xs = case u xs of
            Nothing -> Nothing
            Just (xs', g) -> case v xs' of
                Nothing -> Nothing
                Just (xs'', x) -> Just (xs'', g x)

instance Alternative (Parser tok) where
    empty :: Parser tok a
    empty = Parser $ const Nothing

    (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
    Parser u <|> Parser v = Parser f where
        f xs = case u xs of
            Nothing -> v xs
            z -> z
