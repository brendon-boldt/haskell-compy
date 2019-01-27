module Lex (
  SrcLoc,
  chadLex,
  handleLex,
  Token (Token),
  TData (..),
  TType (..),
) where

import Data.Maybe
import Data.Char
import Text.Read
import Data.Typeable
import qualified Control.Exception as E

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

data SrcLoc = SrcLoc {line :: Int, col :: Int}
 
instance Show SrcLoc where
  show (SrcLoc line col) = (show $ line) ++ ":" ++ (show $ col)

data Token = Token TType TData

data TData = TData {value :: String, loc :: SrcLoc}

data TType =
  Keyword |
  NumLit |
  Name |
  Whitespace |
  InvalidToken 
    deriving (Show, Eq)

keywords = ["let", "be", "show", "."]
isWhitespace s = all (==' ') s

appendLoc s sl = s ++ "@" ++ (show sl)

instance Show Token where
  show (Token Keyword (TData s sl)) = appendLoc ("<" ++ s ++ ">") sl
  show (Token NumLit (TData x sl)) = appendLoc (show x) sl
  show (Token Name (TData s sl)) = appendLoc (show s) sl
  show (Token Whitespace (TData s sl)) = appendLoc ("[ws]") sl
  show (Token InvalidToken (TData s sl)) = appendLoc ("~" ++ s ++ "~") sl

--tokenFolder :: [String] -> Char -> [String]
tokenFolder :: Char -> [String] -> [String]
--tokenFolder prevs cur =
tokenFolder cur prevs =
  let prevWord = (head prevs)
  in case prevWord of
    []  -> [cur] : prevs
    ' ':_ -> case cur of
      ' ' -> (' ' : prevWord) : (tail prevs)
      --'.' -> "." : prevs
      _   -> [cur] : prevs
    "." -> [cur] : prevs
    _   -> case cur of
      ' '  -> " " : prevs
      '.' -> "." : prevs
      char -> (char : prevWord) : (tail prevs)
    
tokenize :: String -> [[String]]
tokenize src =
  --let tokens = map (init . foldl tokenFolder [""]) (lines src)
  let tokens = map (foldr tokenFolder [""]) (lines src)
  --in map (reverse . map reverse) tokens
  in tokens

toToken :: String -> SrcLoc -> Token
toToken s sl
  | (map toLower s) `elem` keywords = Token Keyword (TData s sl)
  | isJust (readMaybe s :: Maybe Int) = Token NumLit (TData s sl)
  | s `elem` (map (\x -> [x]) ['a'..'z']) = Token Name (TData s sl)
  | isWhitespace s = Token Whitespace (TData s sl)
  | otherwise = Token InvalidToken (TData s sl)

addNumbers :: [[String]] -> [Token]
addNumbers strs = 
  let incCol (Token _ t) = (length $ value t) + (col $ loc t)
      f prevs (t, lineNum) = case prevs of
        [] -> toToken t (SrcLoc lineNum 1) : prevs
        _  -> toToken t (SrcLoc lineNum (incCol (head prevs))) : prevs
      addLines strs = map (\(x, y) -> zip x (cycle [y])) (zip strs [1..])
  in concat $ map (reverse . foldl f ([]::[Token])) (addLines strs)

chadLex :: String -> [Token]
chadLex src = addNumbers (tokenize src)

isInvalidToken :: Token -> Bool
isInvalidToken (Token InvalidToken _) = True
isInvalidToken _ = False

formatInvalidTokens :: [Token] -> String
formatInvalidTokens tokens = 
  let invalidTokens = filter isInvalidToken tokens
      initString = "The following invalid tokens were found:\n"
      folder x (Token _ y) = x ++ "\"" ++ (value y) ++ "\" at " ++ (show $ loc y) ++ "\n"
  in foldl folder initString invalidTokens

handleLex :: [Token] -> IO ()
handleLex tokens =
  if' (any isInvalidToken tokens)
    (fail (formatInvalidTokens tokens))
    (putStr "Lexing complete.\n")
