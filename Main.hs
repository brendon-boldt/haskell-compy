import System.IO
import System.Environment
import Data.Maybe

import Lex
import Parse

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

main = do
  filename <- getArgs
  handle <- openFile (head filename) ReadMode
  contents <- hGetContents handle
  let tokens = chadLex contents
  --putStr (show tokens)
  handleLex tokens
  let parseRes = parse tokens
  --putStrLn $ if' (isJust parseRes) ("("++(show (fromJust parseRes))++")") "Parse failed."
  putStrLn $ showParse parseRes
  hClose handle
