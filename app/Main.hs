import System.IO
import System.Environment
import Data.Maybe
import Control.Exception (assert)

import qualified Lex as Lex
import qualified Parse as Parse
import qualified AST as AST
import qualified CodeGen as CG
--import qualified Grammar as G

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

main = do
  filename <- getArgs
  handle <- openFile (head filename) ReadMode
  contents <- hGetContents handle
  let tokens = Lex.chadLex contents
  --putStr (show tokens)
  Lex.handleLex tokens
  --let rawCst = head $ Parse.parse tokens
  --let cst = assert (not $ null $ Parse.nodes rawCst) rawCst
  let cst = head $ Parse.parse tokens
  -- Required for catch parse errors (as of now)
  putStr $ Parse.showPS cst
  let ast = AST.buildAST (head $ Parse.nodes cst)
  --putStrLn $ show ast
  putStrLn ""
  CG.generateAsm ast
  hClose handle
