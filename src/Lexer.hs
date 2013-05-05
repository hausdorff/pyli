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
           -- "Helper" tokens, mostly to help us process literals
           | StrLit String
           | StrIntLit String String  -- (multiline strings)
           | RStrLit String
           | RStrIntLit String String -- (multiline r-strings)
           | CmplxLit String
           | BinLit String
           | HexLit String
           | OctLit String
           | LineCont
           deriving (Eq)
instance Show Tkn where show = dispTkn  -- tells how to display tokens

-- turns tokens into strings
dispTkn :: Tkn -> String
-- "Core" tokens
dispTkn (Newline)        = "(NEWLINE)"
dispTkn (Indent)         = "(INDENT)"
dispTkn (Dedent)         = "(DEDENT)"
dispTkn (Id x)           = concat ["(ID \"", x, "\")"]
dispTkn (Keyword x)      = concat ["(KEYWORD ", x, ")"]
dispTkn (Literal x)      = concat ["(LIT ", x, ")"]
dispTkn (Punct x)        = concat ["(PUNCT \"", x, "\")"]
dispTkn (Error x)        = concat ["(ERROR \"", x, "\")"]
dispTkn (Endmarker)      = "(ENDMARKER)"
dispTkn (Comment)        = "(COMMENT)"
dispTkn (LineCont)       = "(LINECONT)"
-- "Helper" tokens
dispTkn (StrLit x)       = concat ["(LIT \"", x, "\")"]
dispTkn (StrIntLit x _)  = concat ["(STRING \"", x, "\")"]
dispTkn (RStrLit x)      = concat ["(rLIT \"", x, "\")"]
dispTkn (RStrIntLit x _) = concat ["(rLITint \"", (escapeBackslash x), "\")"]
dispTkn (CmplxLit x)     = concat ["(LIT ", x, ")"]
dispTkn (BinLit x)       = concat ["(LIT ", x, ")"]
dispTkn (HexLit x)       = concat ["(LIT ", x, ")"]
dispTkn (OctLit x)       = concat ["(LIT ", x, ")"]



-- LEXER API

-- OUTPUT. Takes a list of tokens, prints each on its own line
emit :: [Tkn] -> IO ()
emit tkns = mapM_ print tkns

-- INPUT. Takes a string and lexes it
lex :: String -> [Tkn]
lex input = joinStrLits (convertRStrLits $ lexlines [0] [] $ lines input) []



-- LEXER CONTROL LOGIC

-- lexes all the lines in a file
-- indent stack -> parens stack -> lines in file -> tokens
lexlines :: [Int] -> [String] -> [String] -> [Tkn]
lexlines [] _ _ =
  -- the indent stack will always have at least one element inside, sometimes
  -- a 0; if it's not there, we error
  error "Indent stack can't be empty"
lexlines istack pstack (s:ss) =
  concat [tkns, nlTkn, lexlines istack' pstack'' ss]
  where
    (tkns, istack', pstack') = lexln istack pstack s
    nlTkn                    = nlTknUpdt pstack' tkns
    pstack''                 = pstackUpdt tkns pstack'
lexlines istack pstack []
  -- trailing parenthesis results in error
  | "\\" `elem` pstack = [Error "trailing backslash not allowed in file"]
  | istack == [0]      = [Endmarker]
  | otherwise          = (fst $ dedentStack istack 0) ++ [Endmarker]


-- lexes one line
-- indent stack -> parens stack -> a single line -> tokens ->
--    (tokens, new indent stack, new parens stack)
lexln :: [Int] -> [String] -> String -> ([Tkn], [Int], [String])
lexln istack [] s = (concat [itkns, tkns'], istack', pstack')
  -- lex indents
  where
    noWs             = dropWhile isSpace s
    curridts         = length $ takeWhile isSpace s
    (tkns, pstack)   = tknizeString noWs []
    (tkns', pstack') = stackUpdtWIndents tkns pstack
    -- INDENT tokens should not be emitted if line is empty
    (itkns, istack') | null tkns' = ([], istack)
                     | otherwise  = updateStack istack curridts
lexln istack pstack s  = (tkns', istack, pstack'')
  -- don't lex indents
  where
    (tkns, pstack')   = tknizeOrLexCont pstack s
    (tkns', pstack'') = stackUpdtWOIndents tkns pstack'

-- if parenthesis stack is empty, there is no line continuation.
-- if line cont is in tokens and not in paren stack, remove from tokens
-- if line cont is in tokens and not in paren stack, push to stack
-- if line cont is in paren stack and not in tokens, remove from stack
stackUpdtWOIndents :: [Tkn] -> [String] -> ([Tkn], [String])
stackUpdtWOIndents tkns pstack = case (LineCont `elemIndex` tkns) of
  Just x  -> isEol x
  Nothing -> (tkns' False False, pstack' False) --(False, Comment)
  where
    isEol n | n == ((length tkns) - 1) = (tkns' True False, pstack' True) --(True, Comment)
            | otherwise                = (tkns' False True, pstack' False) --(False, Error "Backslash in the wrong place")
    lcInPstack = "\\" `elem` pstack
    tkns' lcAtEol err | lcAtEol && lcInPstack ||
                        null pstack = delete LineCont tkns
                      | err         = (Error "backslash must be at eol") : tkns
                      | otherwise   = tkns
    pstack' lcAtEol | lcAtEol && not lcInPstack = "\\" : pstack
                    | not lcAtEol && lcInPstack = delete "\\" pstack
                    | otherwise                 = pstack

-- updates token and pstack states based on whether line continuation was (1)
-- present, and (2) at the EOL
stackUpdtWIndents :: [Tkn] -> [String] -> ([Tkn], [String])
stackUpdtWIndents tkns pstack = case (LineCont `elemIndex` tkns) of
  -- if there is no line continuation, return
  -- line continuations that do not occur at EOL result in error token
  -- line continuations at EOL result in normal return
  Just x  -> isEol x
  Nothing -> (tkns, pstack)
  where isEol n | n == ((length tkns) - 1) =
                    (delete LineCont tkns, "\\" : pstack)
                | otherwise =
                    ((Error "backslash must be at EOL") : tkns, pstack)

-- either tokenizes input string outright, or "continues" lexing b/c the
-- previous line was a multiline string
tknizeOrLexCont :: [String] -> String -> ([Tkn], [String])
tknizeOrLexCont pstack s
  | head pstack == "\\"           = tknizeString s (tail pstack)
  | head pstack `elem` strDelim   = lexcont s pstack
  | otherwise                     = tknizeString s pstack
  where strDelim = ["\'", "'''", "\"", "\"\"\"",
                    "r'", "r'''", "r\"", "r\"\"\""]



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
nlTknUpdt ["\\"] _    = []  -- escaped nl
nlTknUpdt (_:_) (_:_) = []  -- nested parentheses, then new nl
nlTknUpdt [] (x:xs)   = case last (x:xs) of
  StrIntLit _ _  -> []
  RStrIntLit _ _ -> []
  _              -> [Newline]  -- any token but long string is a nl

