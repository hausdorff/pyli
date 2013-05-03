-- TODO: change comments, change imports, fix interstr stuff appearing near the bottom

module Lexer where

import Regex
import LexerUtils

import Data.Char (chr, isSpace)
import Numeric (readOct, readHex)
import Data.List (stripPrefix, elemIndex, delete)
import Data.Maybe (catMaybes)


-- TOKEN LOGIC; all logic relating to the tokens themselves
--
-- Guidance for defining tokens was provided by section 2 of the lexical spec,
-- (and in particular, section 2.2), as well as Matthew Might, who is eminently
-- useul as a compilers person.
data Tkn =
           -- "Core" tokens
             Newline
           | Indent
           | Dedent
           | Id String
           | Keyword String
           | Literal String
           | Punct String
           | Error String
           | Endmarker
           | Comment
           | LineCont
           -- "Helper" tokens, mostly to help us process literals
           | StrLit String
           | StrIntLit String String -- intermediate string for multiline strings
           | RStrLit String
           | RStrIntLit String String-- intermediate raw string for linecont
           | CmplxLit String
           | BinLit String
           | HexLit String
           | OctLit String
           deriving (Eq)
instance Show Tkn where show = dispTkn

-- For displaying the tokens
dispTkn :: Tkn -> String
-- "Core" tokens
dispTkn (Newline)   = "(NEWLINE)"
dispTkn (Indent)    = "(INDENT)"
dispTkn (Dedent)    = "(DEDENT)"
dispTkn (Id x)      = concat ["(ID \"", x, "\")"]
dispTkn (Keyword x) = concat ["(KEYWORD ", x, ")"]
dispTkn (Literal x) = concat ["(LIT ", x, ")"]
dispTkn (Punct x)   = concat ["(PUNCT \"", x, "\")"]
dispTkn (Error x)   = concat ["(ERROR \"", x, "\")"]
dispTkn (Endmarker) = "(ENDMARKER)"
dispTkn (Comment)   = "(COMMENT)"
dispTkn (LineCont)  = "(LINECONT)"
-- "Helper" tokens
dispTkn (StrLit x)        = concat ["(LIT \"", x, "\")"]
dispTkn (StrIntLit x _) = concat ["(STRING \"", x, "\")"]
dispTkn (RStrLit x)       = concat ["(rLIT \"", x, "\")"]
dispTkn (RStrIntLit x _)  = concat ["(rLITint \"", (escapeBackslash x), "\")"]
dispTkn (CmplxLit x)      = concat ["(LIT ", x, ")"]
dispTkn (BinLit x)        = concat ["(LIT ", x, ")"]
dispTkn (HexLit x)        = concat ["(LIT ", x, ")"]
dispTkn (OctLit x)        = concat ["(LIT ", x, ")"]



-- LEXER CONTROL LOGIC

-- outputs a set of tokens, each on its own line
emit :: [Tkn] -> IO ()
emit tkns = mapM_ print tkns

