import System.IO (hGetContents, stdin)
import qualified Lexer as Lexer

main :: IO ()
main = (hGetContents stdin) >>= Lexer.emit . Lexer.lex