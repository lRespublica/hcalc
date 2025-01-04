module HCalc.Useful.Parsers where

import Data.Char
import Data.List
import Data.Maybe

import Control.Applicative
import Control.Monad

import HCalc.Utils.Token
import HCalc.Utils.HNumbers
import HCalc.Utils.Parser
import HCalc.Useful.Grammar

char x = satisfy (==x)

string :: String -> Parser Char String
string = foldr (\x y -> (:) <$> char x <*> y) (pure "")

anySymb = satisfy (const True)
digit = satisfy isDigit
space = satisfy isSpace
operator = satisfy isAvailableOperator
bracket = satisfy isBracket
lBracket = string "("
rBracket = string ")"
comma = string ","

num :: Parser Char String
num = (++) <$> some digit <*> ((:) <$> char '.' <*> some digit) <|> some digit

-- Parser that gets all brackets from expression
allBrackets = (:) <$> bracket <*> allBrackets <|> anySymb *> allBrackets <|> pure []

grammarTokens :: [(Token, Parser Char String)]
grammarTokens = [(L_BRACKET, lBracket), (R_BRACKET, rBracket), (COMMA, comma)]

numberTokens :: [(Token, Parser Char String)]
numberTokens = [(NUM hNan, num)]

operatorsTokens :: [(Token, Parser Char String)]
operatorsTokens = funcStringToParsable <$> operatorStringsList
    where
    funcStringToParsable (strVal, func) = (FUNCTION func, string strVal)

type StringAfterParsing = String
type StringToParse = String

runTokenParser :: (Token, Parser Char String) -> StringToParse -> Maybe (Token, StringAfterParsing)
runTokenParser _ [] = Just (END, [])

-- A separate implementation is needed for numbers, because the Token
-- constructor does not contain information about the number.
-- By default it contains NaN.
runTokenParser (NUM _, parser) str = do
                                   (remaining, readed) <- runParser parser str
                                   let readedAsHNumToken = NUM $ readHNum readed
                                   return (readedAsHNumToken, remaining)

-- We do not need a separate implementation for functions,
-- because the pair (Token FUNCTION f, String Representation) already
-- contains information about function f.
-- Therefore, we calmly return the token with meaningful information.
runTokenParser (token, parser) str = do
                                     (remaining, _) <- runParser parser str
                                     return (token, remaining)

allParsableTokens = join [grammarTokens, numberTokens, operatorsTokens]

-- Running all parsers with folding
parseToken :: String -> Either String (Token, StringAfterParsing)
parseToken str@(s:_) = maybe (Left ("Unknown symbol " ++ [s])) Right $ foldr (\x y -> y <|> runTokenParser x str) Nothing allParsableTokens

parseStringForTokens :: String -> Either String [Token]
parseStringForTokens str = do
                            (newToken, nextStr) <- parseToken str
                            case newToken of
                                END -> return [END]
                                otherToken -> do nextTokens <- parseStringForTokens nextStr
                                                 return (newToken : nextTokens)
