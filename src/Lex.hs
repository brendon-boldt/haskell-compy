module Lex (
  SrcLoc,
  chadLex,
  handleLex,
  Token (Token),
  TData (..),
  TType (..),
) where

import Debug.Trace

import Data.Maybe
import Data.Char
import Text.Read (readMaybe)
import Data.Typeable
import qualified Control.Exception as E

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

data SrcLoc = SrcLoc {line :: Int, col :: Int}
  deriving (Eq, Ord)
 
instance Show SrcLoc where
  show (SrcLoc line col) = (show $ line) ++ ":" ++ (show $ col)

data Token = Token {ttype :: TType, tdata :: TData}

data TData = TData {value :: String, loc :: SrcLoc}

data TType =
  Keyword |
  Symbol |
  NumLit |
  Name |
  Whitespace |
  Comment |
  EOF |
  InvalidToken 
    deriving (Show, Eq)

keywords = ["let", "be", "show", "is", "does", "do", "given",
            "greater", "less", "than", "when"]
symbols =  "*/,.+-'`()"

isWhitespace s = all (==' ') s

appendLoc s sl = s ++ "@" ++ (show sl)

instance Show Token where
  show (Token Comment (TData s sl)) = appendLoc s sl
  show (Token Keyword (TData s sl)) = appendLoc ("<" ++ s ++ ">") sl
  show (Token Symbol (TData s sl)) = appendLoc ("<" ++ s ++ ">") sl
  show (Token NumLit (TData x sl)) = appendLoc (show x) sl
  show (Token Name (TData s sl)) = appendLoc (show s) sl
  show (Token Whitespace (TData s sl)) = appendLoc ("[ws]") sl
  show (Token InvalidToken (TData s sl)) = appendLoc ("~" ++ s ++ "~") sl
  show (Token Lex.EOF (TData _ sl)) = appendLoc ("EOF") sl

tokenFolder :: Char -> [String] -> [String]
tokenFolder cur prevs =
  let prevWord = (head prevs)
  in case prevWord of
    []  -> [cur] : prevs
    ' ':_ -> case cur of
      ' ' -> (' ' : prevWord) : (tail prevs)
      _   -> [cur] : prevs
    s:_ | s `elem` symbols -> [cur] : prevs
    _   -> case cur of
      ' '  -> " " : prevs
      s | s `elem` symbols -> [s] : prevs
      char -> (char : prevWord) : (tail prevs)

tokenize :: String -> [[String]]
tokenize src =
  --let tokens = map (init . foldl tokenFolder [""]) (lines src)
  let tokens = map (foldr tokenFolder [""]) (lines src)
  --in map (reverse . map reverse) tokens
  in tokens

isName (a:b:[]) = (a `elem` "fip") && (b `elem` ['A'..'Z'])
isName _ = False

toToken :: String -> SrcLoc -> Token
toToken s sl
  | null s = Token Whitespace (TData "" sl)
  | (map toLower s) `elem` keywords = Token Keyword (TData s sl)
  | (head s) `elem` symbols = Token Symbol (TData s sl)
  | isJust (readMaybe s :: Maybe Int) = Token NumLit (TData s sl)
  -- | s `elem` (map (\x -> [x]) ['a'..'z']) = Token Name (TData s sl)
  | isName s = Token Name (TData s sl)
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

addEOF :: [Token] -> [Token]
addEOF tokens = do
  let lt = last tokens
  let eofCol = (col $ loc $ tdata lt) + (length $ value $ tdata lt)
  let eofData = TData "" (SrcLoc (line $ loc $ tdata lt) eofCol)
  let eofToken = Token Lex.EOF eofData
  tokens ++ [eofToken]

getTDValue :: Token -> String
getTDValue (Token _ td) = value td

getTDLoc :: Token -> SrcLoc
getTDLoc (Token _ td) = loc td

commentFolder :: Token -> [Token] -> [Token]
commentFolder t prevs@(ph@(Token Comment _):pt)
  | (head $ getTDValue ph) == '(' = t : prevs
  | otherwise = (Token Comment (TData ((getTDValue t) ++ (getTDValue ph)) (getTDLoc t))) : pt
commentFolder t@(Token _ td) (ph:pt) | (value td) == ")" = 
    (Token Comment (TData ")" (loc td))) : pt
commentFolder t prevs = t : prevs

squashComments :: [Token] -> [Token]
squashComments tokens = foldr commentFolder [] tokens

chadLex :: String -> [Token]
chadLex src = do
  let allTokens = addEOF $ addNumbers (tokenize src)
  let squashed = squashComments allTokens
  squashed

isInvalidToken :: Token -> Bool
isInvalidToken (Token InvalidToken _) = True
isInvalidToken _ = False

formatInvalidTokens :: [Token] -> String
formatInvalidTokens tokens = 
  let invalidTokens = filter isInvalidToken tokens
      initString = "The following invalid tokens were found:\n"
      folder x (Token _ y) = x ++ "\"" ++ (value y) ++ "\" at " ++ (show $ loc y) ++ "\n"
  in foldl folder initString invalidTokens

-- TODO catch unmatched comment

handleLex :: [Token] -> IO ()
handleLex tokens =
  if' (any isInvalidToken tokens)
    (fail (formatInvalidTokens tokens))
    (putStr "Lexing complete.\n")
