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

binaryNumPrefixRx :: Regex
binaryNumPrefixRx = oneOf "0" <.> oneOf "bB"

nonzeroDecimalDigit :: String
nonzeroDecimalDigit = asciiTbl [49..57]

decimalDigit :: String
decimalDigit = asciiTbl [48..57]

hexDigit :: String
hexDigit = (++) decimalDigit $ asciiTbl ([97..102] ++ [65..70])

hexNumPrefixRx :: Regex
hexNumPrefixRx = oneOf "0" <.> oneOf "xX"

octalDigit :: String
octalDigit = asciiTbl [48..55]

octalNumPrefixRx :: Regex
octalNumPrefixRx = oneOf "0" <.> oneOf "oO"

-- integer regexes
binaryNumRx :: Regex
binaryNumRx = (<.>) binaryNumPrefixRx $ plus $ oneOf binaryDigit

decimalNumRx :: Regex
decimalNumRx = ((oneOf nonzeroDecimalDigit <.> star (oneOf decimalDigit))
                   <|> (plus (seqOf "0")))

hexNumRx :: Regex
hexNumRx = (<.>) hexNumPrefixRx $ plus $ oneOf hexDigit

octalNumRx :: Regex
octalNumRx = (<.>) octalNumPrefixRx $ plus $ oneOf octalDigit

-- useful helpers for floating point literal regexes
decimalDigitRx :: Regex
decimalDigitRx = plus $ oneOf decimalDigit

expPrefixRx :: Regex
expPrefixRx = oneOf "eE" <.> (op $ oneOf "+-")

expRx :: Regex
expRx = expPrefixRx <.> decimalDigitRx

fractionalRx :: Regex
fractionalRx = oneOf "." <.> decimalDigitRx

pointfloatRx :: Regex
pointfloatRx = ((op decimalDigitRx) <.> fractionalRx <|>
                   (decimalDigitRx <.> oneOf "."))

-- floating point literal regexes
floatNumRx :: Regex
floatNumRx = pointfloatRx <|> expRx

-- imaginary number literal regexes
complexNumRx :: Regex
complexNumRx = (floatNumRx <|> decimalDigitRx) <.> oneOf "jJ"

-- comment regexes
commentRx :: Regex
commentRx = oneOf "#"

-- operator regexes
operatorRx :: Regex
operatorRx = oneOf "+-*/%&|^~<>"
            <|> seqOf "**"
            <|> seqOf "//"
            <|> seqOf "<<"
            <|> seqOf ">>"
            <|> seqOf "<="
            <|> seqOf ">="
            <|> seqOf "=="
            <|> seqOf "!="

-- delimiter regex
delimiterRx :: Regex
delimiterRx = oneOf "()[]{},:.;@="
             <|> seqOf "+="
             <|> seqOf "-="
             <|> seqOf "*="
             <|> seqOf "/="
             <|> seqOf "//="
             <|> seqOf "%="
             <|> seqOf "&="
             <|> seqOf "|="
             <|> seqOf "^="
             <|> seqOf ">>="
             <|> seqOf "<<="
             <|> seqOf "**="

-- punctuation regex
punctRx :: Regex
punctRx = operatorRx <|> delimiterRx

-- python keywords regex
kwRx :: Regex
kwRx = seqOf "False"
   <|> seqOf "None"
   <|> seqOf "True"
   <|> seqOf "and"
   <|> seqOf "as"
   <|> seqOf "assert"
   <|> seqOf "break"
   <|> seqOf "class"
   <|> seqOf "continue"
   <|> seqOf "def"
   <|> seqOf "del"
   <|> seqOf "elif"
   <|> seqOf "else"
   <|> seqOf "except"
   <|> seqOf "finally"
   <|> seqOf "for"
   <|> seqOf "from"
   <|> seqOf "global"
   <|> seqOf "if"
   <|> seqOf "import"
   <|> seqOf "in"
   <|> seqOf "is"
   <|> seqOf "lambda"
   <|> seqOf "nonlocal"
   <|> seqOf "not"
   <|> seqOf "or"
   <|> seqOf "pass"
   <|> seqOf "raise"
   <|> seqOf "return"
   <|> seqOf "try"
   <|> seqOf "while"
   <|> seqOf "with"
   <|> seqOf "yield"