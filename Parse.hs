module Parse (
  Sym (..),
  Node (..),
  parse,
  showParse,
  findError,
) where

import System.IO
import Data.Array
import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Set as Set

import Debug.Trace

import qualified Lex as Lex

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

data Node = Term Lex.Token | NonTerm (Sym, Sym)

instance Show Node where
  show (Term t) = "(" ++ (Lex.value t) ++ ")"
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
-}

--instance Show Sym where
--  show (S n) = "S" ++ (show n) ++ ""

gen :: Sym -> Sym -> [Sym]
gen x@(St _)    y@(StL _)    = map ($ NonTerm (x, y)) [S, StL]
gen x@(Ex _)    y@(Dot _)    = map ($ NonTerm (x, y)) [S, St, StL]
gen x@(Let _)   y@(ExLet1 _) = map ($ NonTerm (x, y)) [Ex]
gen x@(Name _)  y@(ExLet2 _) = map ($ NonTerm (x, y)) [ExLet1]
gen x@(Be _)    y@(Ex _)     = map ($ NonTerm (x, y)) [ExLet2]
gen x@(SShow _) y@(Ex _)     = map ($ NonTerm (x, y)) [Ex]
gen _ _ = []

genTerm :: Lex.Token -> [Sym]
genTerm t@(Lex.Name _ _) = map ($ Term t) [Name, Ex]
genTerm t@(Lex.Keyword s _)
  | s == "." = map (\x -> x $ Term t) [Dot]
  | (map toLower s) == "be" = map ($ Term t) [Be]
  | (map toLower s) == "let" = map ($ Term t) [Let]
  | (map toLower s) == "show" = map ($ Term t) [SShow]
genTerm t@(Lex.NumLit _ _) = map ($ Term t) [NumLit, Ex]

-- http://christos-c.com/treeviewer/#

genFromList :: [Sym] -> [Sym] -> [Sym]
genFromList xs ys = concat [gen x y | x <- xs, y <- ys]

build' :: [Array Int [Sym]] -> Int -> Int -> [Sym]
build' arr l s =
  let f p = genFromList (arr !! (l-p-1) ! s) (arr !! (p-1) ! (s+p))
  in concatMap f [1..l-1]

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

--parse :: [Lex.Token] -> Maybe Sym
parse :: [Lex.Token] -> [Array Int [Sym]]
parse allTokens = do
  let tokens = filterParseable allTokens
  let nWords = length tokens
  let terminals = array (0, nWords-1) (zip [0..nWords-1] (map genTerm tokens))
  let res = build [terminals] 2
  -- TODO make this search for start symbol
  --if' (null (head res ! 0)) Nothing (Just $ head $ head res ! 0)
  res

showParse arr = do
  let showRow row = foldl (\s i -> s ++ (show $ row!i) ++ "\n") "" [0..snd $ bounds row]
  foldl (\s r -> s ++ (showRow r) ++ "\n") "" arr

oldShowParse arr = do
  let showRow row = foldl (\s i -> s ++ (show $ row!i) ++ "\t") "" [0..snd $ bounds row]
  foldl (\s r -> s ++ (showRow r) ++ "\n") "" arr

-- In random places, should I have used recursion instead of range + zip?

isStatement :: Sym -> Bool
isStatement (St _) = True
isStatement (StL _) = True
isStatement _ = False

searchInRow :: Array Int [Sym] -> Set.Set Int -> Int -> Int -> Set.Set Int
searchInRow row tSet i _ | (snd $ bounds $ row) < i = tSet
searchInRow row tSet i n = do
  let hasStatement = any isStatement (row ! i)
  let filtered = Set.filter (\x -> (x < i) || (x >= (i+n))) tSet
  if hasStatement 
    --then searchInRow row (trace (show n) filtered) (i+n) n
    then searchInRow row filtered (i+n) n
    else searchInRow row tSet     (i+1) n

findError' :: [Array Int [Sym]] -> Set.Set Int -> Int -> Set.Set Int
--findError' _ tSet n | (n == 0) || (Set.null tSet) = tSet
findError' [] tSet _ = tSet
findError' (row:table) tSet n = do
  let filtered = searchInRow row tSet 0 n
  findError' table filtered (n-1)


findError table = do
  let nTokens = length table
  let tSet = Set.fromDistinctAscList [0..nTokens-1]
  findError' table tSet nTokens


{- Gameplan for detecting errors.
 - 
 - An "error" will be any string of tokens that cannot be resolved to be part
 - of a statement.
 - Store the index of tokens in a set and remove from the set as we find tokens
 - that are part of a valid statement.
 - Optimize! LATER!
 -} 
