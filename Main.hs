import Text.Read
import System.IO
import System.Environment
import qualified Control.Exception as E

import Lex

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

filterParseable :: [Token] -> [Token]
filterParseable tokens = 
  let shouldKeep (Whitespace _ _) = False
      shouldKeep _ = True
  in filter shouldKeep tokens

main = do
  filename <- getArgs
  handle <- openFile (head filename) ReadMode
  contents <- hGetContents handle
  let tokens = chadLex contents
  --print (tokens)
  handleLex tokens
  hClose handle
