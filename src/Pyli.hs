import System.IO (hGetContents, stdin)
import qualified Lexer as Lexer
import qualified Parser as Parser
import DerpInterface

import Data.Set
import Text.Derp as Derp

-- This `main` allows us to generate a compiler binary, see cabal file
main :: IO ()
main = do code <- (hGetContents stdin)
          let res = toList $ Derp.runParse Parser.file_input $ derpTkns $ Lexer.lex code
          case res of
            [] -> putStrLn "#f"
            x  -> showNL x

showNL :: [String] -> IO()
showNL (t:ts) = do putStrLn t
                   showNL ts
showNL [] = do putStrLn ""