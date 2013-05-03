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
           | StrIntLit String String  -- (multiline strings)
           | RStrLit String
           | RStrIntLit String String -- (multiline r-strings)
           | CmplxLit String
           | BinLit String
           | HexLit String
           | OctLit String
           deriving (Eq)
instance Show Tkn where show = dispTkn  -- tells how to display tokens

-- turns tokens into strings
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
dispTkn (StrIntLit x _)   = concat ["(STRING \"", x, "\")"]
dispTkn (RStrLit x)       = concat ["(rLIT \"", x, "\")"]
dispTkn (RStrIntLit x _)  = concat ["(rLITint \"", (escapeBackslash x), "\")"]
dispTkn (CmplxLit x)      = concat ["(LIT ", x, ")"]
dispTkn (BinLit x)        = concat ["(LIT ", x, ")"]
dispTkn (HexLit x)        = concat ["(LIT ", x, ")"]
dispTkn (OctLit x)        = concat ["(LIT ", x, ")"]



-- LEXER API

-- OUTPUT. Takes a list of tokens, prints each on its own line
emit :: [Tkn] -> IO ()
emit tkns = mapM_ print tkns

-- INPUT. Takes a string and lexes it
lex :: String -> [Tkn]
lex input = joinStrLits (convertRStrLits $ lexlines [0] [] $ lines input) []



-- LEXER CONTROL LOGIC

-- [IndentStack] -> [ParenthesisStack] -> [StringToLex] -> [Tokens]
lexlines :: [Int] -> [String] -> [String] -> [Tkn]
lexlines [] _ _ =
  -- the indent stack will always have at least one element inside, sometimes
  -- a 0; if it's not there, we error
  error "indent stack can't be empty"
lexlines istack pstack (s:ss) =
  concat [tkns, nlTkn, lexlines newistack newpstack ss]
  where
    (tkns, newistack, pstack') = lexLine istack pstack s
    nlTkn                      = nlTknUpdt pstack' tkns
    newpstack                  = pstackUpdt tkns pstack'
lexlines istack pstack []
  -- trailing parenthesis results in error
  | "\\" `elem` pstack = error "trailing backslash"
  | istack == [0]      = [Endmarker]
  | otherwise          = (fst $ dedentStack istack 0) ++ [Endmarker]


-- HELPER FUNCTIONS

-- push e onto stack `st` if it is not in the stack already
pushIfUnique :: Eq a => a -> [a] -> [a]
pushIfUnique e st = case (e `elem` st) of
  True  -> st
  False -> e:st

-- if last token is a multiline string, add open to pstack to track it. ipstack
-- is the intermediate pstack
pstackUpdt :: [Tkn] -> [String] -> [String]
pstackUpdt [] pstack   = pstack
pstackUpdt tkns pstack = case last tkns of
  StrIntLit _ st  -> pushIfUnique st pstack
  RStrIntLit _ st -> pushIfUnique ('r':st) pstack
  _               -> pstack

-- adds newline token to stack if we encounter a non-escaped newline
nlTknUpdt :: [String] -> [Tkn] -> [Tkn]
nlTknUpdt _ []        = []  -- no tokens, then no nl
nlTknUpdt (_:_) (_:_) = []  -- nested parentheses, then new nl
nlTknUpdt ["\\"] _    = []  -- escaped nl
nlTknUpdt [] (x:xs)   = case last (x:xs) of
  StrIntLit _ _  -> []
  RStrIntLit _ _ -> []
  _              -> [Newline]  -- any token but long string is a nl

