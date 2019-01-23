module Parse (
  Sym (..),
  Node (..),
  parse,
  showParse,
) where

import System.IO
import Data.Array
import Debug.Trace
import Data.Maybe
import Data.List
import Data.Char

import qualified Lex as Lex

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

data Node = Term String | NonTerm (Sym, Sym)

instance Show Node where
  show (Term s) = "(" ++ s ++ ")"
  show (NonTerm (l, r)) = "(" ++ (show l) ++ ")(" ++ (show r) ++ ")"

data Sym = S Node |
  StL Node |
  St Node |
  Ex Node |
  ExLet1 Node |
  ExLet2 Node |
  Dot Node |
  Name Node |
  Be Node |
  Let Node |
  SShow Node |
  NumLit Node
  --deriving (Show, Eq)
  deriving (Show)

{-
Raw Grammar
S = StL
StL = St StL
    | St
St = Expr "."
Expr = "let" Nm "be" Expr
     | "show" Expr
     | NumLit
     | Nm

Transformed

--S = StL
StL, S = St StL
St, StL, S = Ex Dot
Ex = Let ExL1
ExL1 = Nm ExL2
ExL2 = Be Expr
Ex = Show Ex

Dot = "."
Nm, Ex = ['a'..'z']
Be = "be"
Let = "let"
SShow = "show"
NumLit, Ex = [0..]
-}

--instance Show Sym where
--  show (S n) = "S" ++ (show n) ++ ""

gen :: Sym -> Sym -> [Sym]
gen (St x) (StL y) = map (\sym -> sym $ NonTerm (St x, StL y)) [S, StL]
gen (Ex x) (Dot y) = map (\sym -> sym $ NonTerm (Ex x, Dot y)) [S, St, StL]
gen (Let x) (ExLet1 y) = map (\sym -> sym $ NonTerm (Let x, ExLet1 y)) [Ex]
gen (Name x) (ExLet2 y) = map (\sym -> sym $ NonTerm (Name x, ExLet2 y)) [ExLet1]
gen (Be x) (Ex y) = map (\sym -> sym $ NonTerm (Be x, Ex y)) [ExLet2]
gen (SShow x) (Ex y) = map (\sym -> sym $ NonTerm (SShow x, Ex y)) [Ex]
gen _ _ = []

-- Eventually, this should gel with Lex
--genTerm :: String -> [Sym]
genTerm :: Lex.Token -> [Sym]
genTerm (Lex.Name s _) = map (\x -> x $ Term s) [Name, Ex]
genTerm (Lex.Keyword s _)
  | s == "." = map (\x -> x $ Term ".") [Dot]
  | (map toLower s) == "be" = map (\x -> x $ Term "be") [Be]
  | (map toLower s) == "let" = map (\x -> x $ Term "let") [Let]
  | (map toLower s) == "show" = map (\x -> x $ Term "show") [SShow]
genTerm (Lex.NumLit n _) = map (\x -> x $ Term n) [NumLit, Ex]

-- http://christos-c.com/treeviewer/#
input = "let a be 4 . let b be let a be 3 . let d be show c ."

genFromList :: [Sym] -> [Sym] -> [Sym]
genFromList xs ys = concat [gen x y | x <- xs, y <- ys]

build' :: [Array Int [Sym]] -> Int -> Int -> [Sym]
build' arr l s = do
  let f = (\p -> genFromList (arr !! (l-p-1) ! s) (arr !! (p-1) ! (s+p)))
  foldl (\x y -> f y ++ x) [] [1..l-1]

build :: [Array Int [Sym]] -> Int -> [Array Int [Sym]]
build arr l = do
  let rowLen = (snd $ bounds $ head arr)
  let res = map (build' arr l) [0..rowLen-1] 
  let newArr = array (0, rowLen-1) (zip [0..rowLen] res) : arr
  if' (rowLen <= 1) newArr (build newArr (l+1))

filterParseable :: [Lex.Token] -> [Lex.Token]
filterParseable tokens = 
  let shouldKeep (Lex.Whitespace _ _) = False
      shouldKeep _ = True
  in filter shouldKeep tokens

parse :: [Lex.Token] -> Maybe Sym
parse allTokens = do
  let tokens = filterParseable allTokens
  let nWords = length tokens
  let terminals = array (0, nWords-1) (zip [0..nWords-1] (map genTerm tokens))
  let res = build [terminals] 2
  -- TODO make this search for start symbol
  if' (null (head res ! 0)) Nothing (Just $ head $ head res ! 0)

showParse arr = do
  let showRow row = foldl (\s i -> s ++ (show $ row!i) ++ "\t") "" [0..snd $ bounds row]
  foldl (\s r -> s ++ (showRow r) ++ "\n") "" arr
