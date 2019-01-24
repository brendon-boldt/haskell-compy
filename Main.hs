import System.IO
import System.Environment
import Data.Maybe

import qualified Lex as Lex
import qualified Parse as Parse

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
  let parseRes = Parse.parse tokens
  --putStrLn $ if' (isJust parseRes) ("("++(show (fromJust parseRes))++")") "Parse failed."
  putStrLn $ Parse.showParse parseRes
  putStrLn $ show $ Parse.findError parseRes
  hClose handle
