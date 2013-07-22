-- A lot of this is copied basically verbatim from the full python grammar
-- located at http://docs.python.org/3/reference/grammar.html
-- For example, the function `fileInput` is actually just a rule in the Python
-- grammar. Nearly everything also appears in the order it does in the grammar.
module Parser (parseFile) where

import Prelude hiding (id)
import Data.String.Utils (join)
import Data.Set (Set)
import Text.Derp

import DerpInterface
import qualified Lexer



-- PUBLIC-FACING PARSER API FUNCTIONS

parseFile :: [Lexer.Tkn] -> Set String
parseFile tkns = runParse fileInput $ derpTkns tkns

-- PRIVATE "HELPER" FUNCTIONS FOR THE PARSER
-- These functions mostly correspond to the grammar rules in the Python
-- grammar, but are effectively useless outside of this module. Thus they are
-- private.

-- corresponds to `file_input` in grammar
fileInput :: Parser String
fileInput = lines <~> endOfFile ==> emitProgram
  where lines     = lineCfg
        endOfFile = ter "ENDMARKER"

lineCfg :: Parser String
lineCfg = emptyLine
          <|> newline <~> moreLines ==> emitNl
          <|> stmt <~> moreLines    ==> emitLine
  where emptyLine = eps ""
        newline = ter "NEWLINE"
        moreLines = lineCfg

-- corresponds to `funcdef` in grammar
funcdef :: Parser String
funcdef = def <~> id <~> parameters <~> colon <~> body ==> emitFuncdef
  where def   = ter "def"
        id    = ter "ID"
        colon = ter ":"
        body  = suite

-- corresponds to `parameters` in grammar
parameters :: Parser String
parameters = openParen <~> paramsCfg <~> closeParen ==> emitParams
  where openParen  = ter "("
        closeParen = ter ")"

paramsCfg :: Parser String
paramsCfg = noParams
            <|> id <~> restOfIds ==> emitParamList
            <|> comma
  where noParams  = eps ""
        id        = ter "ID"
        comma     = ter ","
        restOfIds = noParams
                    <|> comma <~> id <~> restOfIds ==> emitRestOfParams



-- EMISSION FUNCTIONS
-- Functions designed to take the output of a reduction from Derp lib,
-- destructure it using pattern matching, and output a string that is a like
-- a lispy AST.
--
-- Each is accompanied by an emit function that gives hints about how and why
-- the destructuring is happening. For example, `emitFuncdef` has `emitFuncdef'`
-- as a helper, and it takes `id`, `params`, and `body` as parameters, which
-- tells you a lot about how the destructuring works.

emitProgram :: (String,String) -> String
emitProgram (p, _) = emitProgram' p

emitProgram' :: String -> String
emitProgram' p = "(program " ++ p ++ ")"

emitFuncdef :: (String,(String,(String,(String,String)))) -> String
emitFuncdef (_, (id, (params, (_, body)))) = emitFuncdef' id params body

emitFuncdef' :: String -> String -> String -> String
emitFuncdef' id params body = (join " " [("(def (" ++ id), params])
                              ++ ") (" ++ body ++ "))"

emitParams :: (String, (String, String)) -> String
emitParams (_, (ps, _)) = ps

emitParamList :: (String, String) -> String
emitParamList (s1,s2) = join " " [s1,s2]

emitRestOfParams :: (String,(String,String)) -> String
emitRestOfParams (_,(name,_)) = name

emitNl :: (String,String) -> String
emitNl (_,x) = x

emitLine :: (String,String) -> String
emitLine (s,x) = join " " [s,x]

