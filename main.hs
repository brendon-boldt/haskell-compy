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

import qualified Data.Text as T
import Data.Maybe
import Text.Read

-- Begin Lex --

symbols = ".,"
padSymbols src =
  let replacer = (\a -> \b -> T.replace (T.pack [b]) (T.pack [' ',b,' ']) a)
  in foldl replacer src symbols

isWhitespace = \x -> x `elem` "\n\t "

loserLex :: T.Text -> [Token]
loserLex src = map toToken (preLex src)
preLex src =
  filter
    (\x -> not $ T.null x)
    (T.split isWhitespace (padSymbols (T.toLower src)))
    -- This needs to be reegineered to keep track of location


data Token =
  Keyword String |
  NumLit Integer |
  Name String |
  InvalidToken String

instance Show Token where
  show (Keyword s) = "<" ++ s ++ ">"
  show (NumLit x) = show x
  show (Name s) = show s
  show (InvalidToken s) = "~" ++ s ++ "~"

keywords = map T.pack ["let", "be", "show", "."]

toToken :: T.Text -> Token
toToken t
  | t `elem` keywords =
      (Keyword (T.unpack t))
  | isJust (readMaybe (T.unpack t) :: Maybe Integer) =
      (NumLit (read (T.unpack t) :: Integer))
  | (T.unpack t) `elem` (map (\x -> [x]) ['a'..'z']) =
      (Name (T.unpack t))
  | otherwise =
      InvalidToken (T.unpack t)

-- End Lex --

-- Begin Parse --

--dummyParse :: [Token] -> IO ()
--dummyParse = 
--  let f = (\x -> case x of
--    Invalid s = print(

-- End Parse --

main = do
  let res = (loserLex $ T.pack "Let a be 9. Let b not be 23.")
  print res
  --print (all (/= InvalidToken) res)
  --print (map (fromJust) res)
