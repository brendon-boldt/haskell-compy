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

-- Begin Lex --

type SrcLoc = (Integer, Integer)

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
  | isJust (readMaybe s :: Maybe Integer) = NumLit s sl
  | s `elem` (map (\x -> [x]) ['a'..'z']) = Name s sl
  | isWhitespace s = Whitespace s sl
  | otherwise = InvalidToken s sl

addNumbers :: [[String]] -> [Token]
addNumbers strs = 
  let incCol t = fromIntegral (length $ value t) + (snd $ loc t)
      f prevs (t, lineNum) = case prevs of
        [] -> toToken t (lineNum, 1) : prevs
        _  -> toToken t (lineNum, incCol (head prevs)) : prevs
      addLines strs = map (\(x, y) -> zip x (cycle [y])) (zip strs [1..])
  in concat $ map (reverse . foldl f ([]::[Token])) (addLines strs)

chadLex :: String -> [Token]
chadLex src = addNumbers (tokenize src)

isValidLex tokens = 
  let isInvalid (InvalidToken _ _) = True
      isInvalid _ = False
  in not $ all isInvalid tokens

-- End Lex --

-- Begin Parse --

--dummyParse :: [Token] -> IO ()
--dummyParse = 
--  let f = (\x -> case x of
--    Invalid s = print(

-- End Parse --

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


main = do
  filename <- getArgs
  handle <- openFile (head filename) ReadMode
  contents <- hGetContents handle
  --print $ chadLex contents
  let tokens = chadLex contents
  print (tokens)
  if' (isValidLex tokens) (putStr "valid lex\n") (putStr "invalid lex\n")
  hClose handle
