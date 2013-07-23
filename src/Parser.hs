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
            <|> nonlocalStmt
            <|> assertStmt

-- corresponds to `expr_stmt` in grammar
-- augmented assignment is stuff like `+=` and `*=`. A "test" is an assignment
-- with a conditional, eg, `x = 1 if 1 == y else 2`.

-- TODO: REFACTOR THIS TO USE DIFFERENT FUNCTIONS AND STUFF
exprStmt :: Parser String
exprStmt = testlist <~> augassign <~> testlist ==> emitAugAssignStmt
           <|> testlist <~> equals <~> tuple_or_test ==> emitAssignStmt
           <|> tuple_or_test ==> emitExprStmt
    where equals = ter "="

-- corresponds to `augassign` in grammar
augassign :: Parser String
augassign = ter "+="
            <|> ter "-="
            <|> ter "*="
            <|> ter "/="
            <|> ter "%="
            <|> ter "&="
            <|> ter "|="
            <|> ter "^="
            <|> ter "<<="
            <|> ter ">>="
            <|> ter "**="
            <|> ter "//="

-- corresponds to `del_stmt` in grammar
delStmt :: Parser String
delStmt = del <~> star_expr ==> emitDelStmt
  where del = ter "del"

-- corresponds to `pass_stmt` in grammar
passStmt :: Parser String
passStmt = pass ==> emitPassStmt
  where pass = ter "pass" 

-- corresponds to `flow_stmt` in grammar
-- TODO: UPDATE THIS FUNCTION
flow_stmt :: Parser String
flow_stmt = breakStmt <|> continueStmt <|> returnStmt <|> raiseStmt

-- corresponds to `break_stmt` in grammar
breakStmt :: Parser String
breakStmt = break ==> emitBreakStmt
  where break = ter "break"

-- corresponds to `continue_stmt` in grammar
continueStmt :: Parser String
continueStmt = continue ==> emitContinueStmt
  where continue = ter "continue"

-- corresponds to `return_stmt` in grammar
-- TODO: REFACTOR THE MIDDLE PART OF THE RULE BELOW
returnStmt :: Parser String
returnStmt = return <~> (eps "" <|> testlist) ==> emitReturnStmt
  where return = ter "return"

-- corresponds to `raise_stmt` in grammar
-- TODO: REFACTOR THIS WHOLE FUNCTION
raiseStmt :: Parser String
raiseStmt =     raise
            <~> (eps "" <|> (test <~>
                             (eps "" <|> from <~> test ==> (\(_,t) -> t))
                              ==> (\(t,f)-> join " " [t,f]))) 
                 ==> (\(s1,s2) -> join " " [s1,s2])
  where raise = ter "raise"
        from  = ter "from"

-- corresponds to `global_stmt` in grammar
globalStmt :: Parser String
globalStmt = global <~> id <~> zeroPlusIds ==> emitGlobalStmt
  where global = ter "global"
        id     = ter "ID"

zeroPlusIds :: Parser String
zeroPlusIds = noMoreIds
              <|> comma <~> id <~> zeroPlusIds ==> emitRestOfIds
  where noMoreIds = eps ""
        comma     = ter ","
        id        = ter "ID"

-- corresponds to `nonlocal_stmt` in grammar
nonlocalStmt :: Parser String
nonlocalStmt = nonlocal <~> id <~> zeroPlusIds ==> emitNonlocalStmt
  where nonlocal = ter "nonlocal"
        id       = ter "ID"

-- corresponds to `assert_stmt` in grammar
assertStmt:: Parser String
assertStmt = assert <~> test <~> zeroPlusTests ==> emitAssertStmt
  where assert = ter "assert"

zeroPlusTests :: Parser String
zeroPlusTests = noMoreTests <|> (comma <~> test) ==> emitRestTests
  where noMoreTests = eps ""
        comma       = ter ","

-- corresponds to `compound_stmt` in grammar
compoundStmt :: Parser String
compoundStmt = ifStmt
               <|> whileStmt
               <|> forStmt
               <|> tryStmt
               <|> funcdef

-- corresponds to `if_stmt` in grammar
ifStmt :: Parser String
ifStmt = ifKeyword <~> test <~> colon <~> block <~> elseBlock ==> emitIfStmt
  where ifKeyword = ter "if"
        colon     = ter ":"
        block     = suite
        elseBlock = zeroOrMoreElifs <~> elseClause

elseClause :: Parser String
elseClause = eps ""
             <|> elseKeyword <~> colon <~> block ==> emitElseClause
  where elseKeyword = ter "else"
        colon       = ter ":"
        block       = suite

zeroOrMoreElifs :: Parser String
zeroOrMoreElifs = noMoreElifs
                  <|> elif <~> test <~> colon <~> block <~> zeroOrMoreElifs
                  ==> emitElifs
  where noMoreElifs = eps "" 
        elif        = ter "elif"
        colon       = ter ":"
        block       = suite

-- corresponds to `while_stmt` in grammar
whileStmt :: Parser String
whileStmt = while <~> test <~> colon <~> block <~> whileElseClause
            ==> emitWhileStmt
  where while = ter "while"
        colon = ter ":"
        block = suite

whileElseClause :: Parser String
whileElseClause = noElseClause
                  <|> elseKeyword <~> colon <~> block ==> emitWhileElseClause
  where noElseClause = eps ""
        elseKeyword  = ter "else"
        colon        = ter ":"
        block        = suite

-- corresponds to `for_stmt` in grammar
forStmt :: Parser String
forStmt = for <~> id <~> inKeywrd <~> test <~> colon <~> block <~> forElseClause
          ==> emitForStmt
  where for      = ter "for"
        id       = ter "ID"
        inKeywrd = ter "in"
        colon    = ter ":"
        block    = suite

forElseClause :: Parser String
forElseClause = noElseClause
                <|> elseKeyword <~> colon <~> block ==> emitForElseClause
  where noElseClause = eps ""
        elseKeyword  = ter "else"
        colon        = ter ":"
        block        = suite



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

emitRestOfIds :: (String,(String,String)) -> String
emitRestOfIds (_, (x, xs)) = join " " [x, xs]

emitNonlocalStmt :: (String,(String,String)) -> String
emitNonlocalStmt (_, (x, xs)) = joinStrs [nonlocal, exps]
  where nonlocal = "nonlocal "
        exps     = join " " [x, xs]

emitRestTests :: (String,String) -> String
emitRestTests (_, tst) = tst

emitAssertStmt :: (String,(String,String)) -> String
emitAssertStmt (_, (exp1, exp2)) = joinStrs [assert, exps]
  where assert = "assert "
        exps   = join " " [exp1,exp2]

emitIfStmt :: (String,(String,(String,(String,(String,String))))) -> String
emitIfStmt (_,(t,(_,(s,(elif,els))))) = (join " " [(join " " [("(cond (" ++ t ++ " (" ++ s ++ "))"), elif]) , els]) ++ ")"

emitElseClause :: (String,(String,String)) -> String
emitElseClause (_, (_, body)) = joinStrs [header, body, footer]
  where header = "(else ("
        footer = "))"

emitElifs :: (String,(String,(String,(String,String)))) -> String
emitElifs (_, (tk, (_, (cond, block)))) = join " " [header, block]
  where wrappedCond = " (" ++ cond ++ ")"
        header      = "(" ++ tk ++ wrappedCond ++ ")"

emitWhileElseClause :: (String,(String,String)) -> String
emitWhileElseClause (_, (_, s)) = "(" ++ s ++ ")"

emitWhileStmt :: (String,(String,(String,(String,(String))))) -> String
emitWhileStmt (_,(t,(_,(s,(block))))) =  body ++ footer
  where header = "(while " ++ t ++ " (" ++ s ++ ")"
        body   = join " " [header, block]
        footer = ")"

emitForElseClause :: (String,(String,String)) -> String
emitForElseClause (_, (_, block)) = "(" ++ block ++ ")"

emitForStmt :: (String,(String,(String,(String,(String,(String,String))))))
               -> String
emitForStmt (_, (id, (_, (tst, (_, (stmts, els)))))) = joinStrs [forOpen,
                                                                 forLoop,
                                                                 forClose]
  where forOpen = "(for "
        forLoop = join " " [id, tst, "(", stmts, ")", els]
        forClose = ")"

emitTryStmt :: (String,(String,(String,String))) -> String
emitTryStmt (_, (_, (exp, except))) = joinStrs [header, exp', except, footer]
  where header = "(try ("
        exp'   = exp ++ ") "
        footer = ")"

emitExceptClauses :: (String,(String,(String,String))) -> String
emitExceptClauses (exc, (_, (exp, bl))) = join " " [clause, bl]
  where clause = "(" ++ exc ++ " (" ++ exp ++ "))"

emitExceptClause :: (String,String) -> String
emitExceptClause (_,bl) = joinStrs [except, bl, footer]
  where except = "(except "
        footer = ")"

emitSuite :: (String,(String,(String,String))) -> String
emitSuite (_, (_, (stm, _))) = joinStrs [suitek, stm]
  where suitek = "suite "


joinStrs :: [String] -> String
joinStrs ss = join "" ss
