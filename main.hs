{-
 
Program = ExprList
ExprList = Expr "." ExprList 
         | epsilon
Expr = LetExpr
     | ShowExpr
     | NumExpr
LetExpr = "let" Name "be" Expr
ShowExpr = "show" Expr
NumExpr = [0, 1, 2, ...]

-}

import Data.Maybe
import Data.Char
import Text.Read
import System.IO
import System.Environment
import qualified Control.Exception as E

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


-- Begin Lex --

data SrcLoc = SrcLoc {line :: Int, col :: Int}
 
instance Show SrcLoc where
  show (SrcLoc line col) = (show $ line) ++ ":" ++ (show $ col)

data Token =
  Keyword {value :: String, loc :: SrcLoc} |
  NumLit {value :: String, loc :: SrcLoc} |
  Name {value :: String, loc :: SrcLoc} |
  Whitespace {value :: String, loc :: SrcLoc} |
  InvalidToken {value :: String, loc :: SrcLoc}

keywords = ["let", "be", "show", "."]
isWhitespace s = all (==' ') s

appendLoc s sl = s ++ "@" ++ (show sl)

instance Show Token where
  show (Keyword s sl) = appendLoc ("<" ++ s ++ ">") sl
  show (NumLit x sl) = appendLoc (show x) sl
  show (Name s sl) = appendLoc (show s) sl
  show (Whitespace s sl) = appendLoc ("[ws]") sl
  show (InvalidToken s sl) = appendLoc ("~" ++ s ++ "~") sl

tokenFolder :: [String] -> Char -> [String]
tokenFolder prevs cur =
  let prevWord = (head prevs)
  in case prevWord of
    []  -> [cur] : prevs
    ' ':_ -> case cur of
      ' ' -> (' ' : prevWord) : (tail prevs)
      --'.' -> "." : prevs
      _   -> [cur] : prevs
    _   -> case cur of
      ' '  -> " " : prevs
      '.' -> "." : prevs
      char -> (char : prevWord) : (tail prevs)
    
tokenize :: String -> [[String]]
tokenize src =
  let tokens = map (init . foldl tokenFolder [""]) (lines src)
  in map (reverse . map reverse) tokens

toToken :: String -> SrcLoc -> Token
toToken s sl
  | (map toLower s) `elem` keywords = Keyword s sl
  | isJust (readMaybe s :: Maybe Int) = NumLit s sl
  | s `elem` (map (\x -> [x]) ['a'..'z']) = Name s sl
  | isWhitespace s = Whitespace s sl
  | otherwise = InvalidToken s sl

addNumbers :: [[String]] -> [Token]
addNumbers strs = 
  let incCol t = (length $ value t) + (col $ loc t)
      f prevs (t, lineNum) = case prevs of
        [] -> toToken t (SrcLoc lineNum 1) : prevs
        _  -> toToken t (SrcLoc lineNum (incCol (head prevs))) : prevs
      addLines strs = map (\(x, y) -> zip x (cycle [y])) (zip strs [1..])
  in concat $ map (reverse . foldl f ([]::[Token])) (addLines strs)

chadLex :: String -> [Token]
chadLex src = addNumbers (tokenize src)

isInvalidToken (InvalidToken _ _) = True
isInvalidToken  _ = False

formatInvalidTokens :: [Token] -> String
formatInvalidTokens tokens = 
  let invalidTokens = filter isInvalidToken tokens
      initString = "The following invalid tokens were found:\n"
      folder x y = x ++ "\"" ++ (value y) ++ "\" at " ++ (show $ loc y) ++ "\n"
  -- TODO use composition
  in foldl folder initString invalidTokens

handleLex :: [Token] -> IO ()
handleLex tokens =
  if' (any isInvalidToken tokens)
    (putStr (formatInvalidTokens tokens))
    (putStr "Lexing complete.\n")

-- End Lex --

-- Begin Parse --

--dummyParse :: [Token] -> IO ()
--dummyParse = 
--  let f = (\x -> case x of
--    Invalid s = print(

-- End Parse --

main = do
  filename <- getArgs
  handle <- openFile (head filename) ReadMode
  contents <- hGetContents handle
  let tokens = chadLex contents
  --print (tokens)
  handleLex tokens
  hClose handle
