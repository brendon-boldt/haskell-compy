import System.IO
import System.Environment
import Data.Maybe

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
  --putStrLn "Lex start."
  Lex.handleLex tokens
  --putStrLn "Lex done."
  let cst = head $ Parse.parse tokens
  --putStrLn "Parse done."
  --putStrLn $ Parse.showPS cst
  let ast = AST.buildAST (head $ Parse.nodes cst)
  --putStrLn "AST done."
  --putStrLn $ show ast
  CG.generateAsm ast
  --putStrLn "CG done."
  hClose handle
