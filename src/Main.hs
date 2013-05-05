import System.IO (hGetContents, stdin)
import qualified Lexer as Lexer

main :: IO ()
main = do code <- (hGetContents stdin)
          Lexer.emit $ Lexer.lex code