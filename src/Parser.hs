-- A lot of this is copied basically verbatim from the full python grammar
-- located at http://docs.python.org/3/reference/grammar.html
-- For example, the function `fileInput` is actually just a rule in the Python
-- grammar. Nearly everything also appears in the order it does in the grammar.
module Parser (parseFile) where

import Prelude hiding (break, exp, id, lines, return)
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
parameters = openParen <~> zeroPlusParams <~> closeParen ==> emitParams
  where openParen  = ter "("
        closeParen = ter ")"

zeroPlusParams :: Parser String
zeroPlusParams = noParams
                   <|> id <~> restOfIds ==> emitParamList
                   <|> comma
  where noParams  = eps ""
        id        = ter "ID"
        comma     = ter ","
        restOfIds = noParams
                    <|> comma <~> id <~> restOfIds ==> emitRestOfParams

-- corresponds to `stmt` in grammar
stmt :: Parser String
stmt = simpleStmt <|> compoundStmt

-- corresponds to `simple_stmt` in grammar
-- simple statements are either terminated with newline or semicolon
simpleStmt :: Parser String
simpleStmt = smallStmt <~> zeroPlusSmallStmts <~> endOfStmts ==> emitSimpleStmt
  where noMoreStmts = eps ""
        semicolon   = ter ";"
        newline     = ter "NEWLINE"
        endOfStmts  = (noMoreStmts <~> newline)
                      <|> (semicolon <~> newline)

zeroPlusSmallStmts :: Parser String
zeroPlusSmallStmts = noMoreStmts
                     <|> moreSmallStmts
  where noMoreStmts    = eps ""
        semicolon      = ter ";"
        moreSmallStmts = semicolon <~> smallStmt <~> moreSmallStmts
                          ==> emitSmallStmts

-- corresponds to `small_stmt` in grammar
smallStmt :: Parser String
smallStmt = exprStmt
            <|> delStmt
            <|> passStmt
            <|> flow_stmt
            <|> globalStmt
            <|> nonlocal_stmt
            <|> assert_stmt



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
emitProgram' prog = joinStrs [header, prog, footer]
  where header = "(program "
        footer = ")"

emitFuncdef :: (String,(String,(String,(String,String)))) -> String
emitFuncdef (_, (id, (params, (_, body)))) = emitFuncdef' id params body

emitFuncdef' :: String -> String -> String -> String
emitFuncdef' id params body = joinStrs [header, wrappedBody, footer]
  where header      = join " " ["(def (" ++ id, params] ++ ") "
        wrappedBody = "(" ++ body
        footer      = "))"

emitParams :: (String, (String, String)) -> String
emitParams (_, (ps, _)) = ps

emitParamList :: (String, String) -> String
emitParamList (s1, s2) = join " " [s1, s2]

emitRestOfParams :: (String,(String,String)) -> String
emitRestOfParams (_, (name, _)) = name

emitNl :: (String,String) -> String
emitNl (_, ln) = ln

emitLine :: (String,String) -> String
emitLine (sp, exp) = join " " [sp, exp]

emitSmallStmts :: (String, (String, String)) -> String
emitSmallStmts (_, (stmts, end)) = joinStrs [wrappedStmt, end]
  where wrappedStmt = "(" ++ stmts ++ ") "

emitSimpleStmt :: (String, (String, (String, String))) -> String
emitSimpleStmt (st1, (st2, _)) = case (null st2) of
  True  -> "(" ++ st1 ++ ")"
  False -> joinStrs [header, body, footer]
  where header = "(begin (" ++ st1 ++ ") "
        body   = st2
        footer = ")"

emitAssignStmt :: (String, (String, String)) -> String
emitAssignStmt (id, (_, rhs)) = joinStrs [lhs, rhs]
  where lhs = "= (" ++ id ++ ")"

emitExprStmt :: String -> String
emitExprStmt st = joinStrs ["expr ", st]

emitAugAssignStmt :: (String, (String, String)) -> String
emitAugAssignStmt (exp,(aug,rhs)) = joinStrs [operator, lhs, rhs]
  where operator = "\"" ++ aug ++ "\" "
        lhs      = "(" ++ exp ++ ") "

emitDelStmt :: (String,String) -> String
emitDelStmt (_, exp) = joinStrs ["del ", exp]

emitPassStmt :: String -> String
emitPassStmt _ = "pass"

emitBreakStmt :: String -> String
emitBreakStmt _ = "break"

emitContinueStmt :: String -> String
emitContinueStmt _ = "continue"

emitReturnStmt :: (String,String) -> String
emitReturnStmt (_, exp) = join " " ["return", exp]

emitGlobalStmt :: (String,(String,String)) -> String
emitGlobalStmt (_, (x, xs)) = joinStrs [global, exps]
  where global = "global "
        exps   = join " " [x,xs]

-- TODO: REFACTOR
emitRestOfIds :: (String,(String,String))
emitRestOfIds (_,(i,r)) = join " " [i,r]

joinStrs :: [String] -> String
joinStrs ss = join "" ss
