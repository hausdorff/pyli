{- Contains mostly regular expressions for lexing Python. Follows extremely
 - closely with the Python lexical spec:
 - http://docs.python.org/3/reference/lexical_analysis.html -}
module LexerUtils where

import Data.Char

import Regex

-- list of ints from ascii table => string with corresponding ascii chars
asciiTbl :: [Int] -> String
asciiTbl li = map chr li

-- useful helpers for integer literal regexes
binaryDigit :: String
binaryDigit = asciiTbl [48..49]

binaryNumPrefixRegex :: Regex
binaryNumPrefixRegex = oneOf "0" <.> oneOf "bB"

nonzeroDecimalDigit :: String
nonzeroDecimalDigit = asciiTbl [49..57]

decimalDigit :: String
decimalDigit = asciiTbl [48..57]

hexDigit :: String
hexDigit = (++) decimalDigit $ asciiTbl ([97..102] ++ [65..70])

hexNumPrefixRegex :: Regex
hexNumPrefixRegex = oneOf "0" <.> oneOf "xX"

octalDigit :: String
octalDigit = asciiTbl [48..55]

octalNumPrefixRegex :: Regex
octalNumPrefixRegex = oneOf "0" <.> oneOf "oO"

-- integer regexes
binaryNumRegex :: Regex
binaryNumRegex = (<.>) binaryNumPrefixRegex $ plus $ oneOf binaryDigit

decimalNumRegex :: Regex
decimalNumRegex = ((oneOf nonzeroDecimalDigit <.> star (oneOf decimalDigit))
                   <|> (plus (seqOf "0")))

hexNumRegex :: Regex
hexNumRegex = (<.>) hexNumPrefixRegex $ plus $ oneOf hexDigit

octalNumRegex :: Regex
octalNumRegex = (<.>) octalNumPrefixRegex $ plus $ oneOf octalDigit

-- useful helpers for floating point literal regexes
decimalDigitRegex :: Regex
decimalDigitRegex = plus $ oneOf decimalDigit

expPrefixRegex :: Regex
expPrefixRegex = oneOf "eE" <.> (op $ oneOf "+-")

expRegex :: Regex
expRegex = expPrefixRegex <.> decimalDigitRegex

fractionalRegex :: Regex
fractionalRegex = oneOf "." <.> decimalDigitRegex

pointfloatRegex :: Regex
pointfloatRegex = ((op decimalDigitRegex) <.> fractionalRegex <|>
                   (decimalDigitRegex <.> oneOf "."))

-- floating point literal regexes
floatNumRegex :: Regex
floatNumRegex = pointfloatRegex <|> expRegex

-- imaginary number literal regexes
complexNumRegex :: Regex
complexNumRegex = (floatNumRegex <|> decimalDigitRegex) <.> oneOf "jJ"

-- comment regexes
commentRegex :: Regex
commentRegex = oneOf "#"

-- operator regexes
operatorRegex :: Regex
operatorRegex = "+-*/%&|^~<>"