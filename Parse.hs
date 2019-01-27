module Parse (
  Sym (..),
  Node (..),
  parse,
) where

import Debug.Trace
import Data.Maybe
import Data.Char (toLower)

import qualified Lex as Lex

data Sym = S | StL | St | Expr | Nm | NumLit |
  T Lex.TType String |
  T' Lex.TType
  deriving (Show)

data Node = Node Sym [Node] | Leaf Sym Lex.Token
  --deriving (Show)

instance Show Node where
  show (Leaf (T ttype _) (Lex.Token _ td)) =
    "("++(show ttype)++" "++(Lex.value td)++")"
  show (Leaf (T' ttype) (Lex.Token _ td)) =
    "("++(show ttype)++" "++(Lex.value td)++")"
  show (Node s ns) = " ("++(show s)++" "++(concatMap show ns)++")"

-- Parser State
data PS = PS {tokens :: [Lex.Token], nodes :: [Node]}
  deriving (Show)

--type LKW = Lex.Keyword
lKW = Lex.Keyword

-- TODO use a (hash)map
getProds :: Sym -> [[Sym]]
getProds S = [[StL]]
getProds StL = [[St, StL],
                [St]]
getProds St = [[Expr, T lKW "."]]
getProds Expr = [[T lKW "let", T' Lex.Name, T lKW "be", Expr],
                 [T lKW "show", Expr],
                 [T' Lex.NumLit],
                 [T' Lex.Name]]

isValidPS :: PS -> Bool
isValidPS ps = not $ null $ nodes ps

addTerm :: PS -> Sym -> PS
addTerm ps s@(T _ _) = PS (tail $ tokens ps) ((Leaf s (head $ tokens ps)): (nodes ps))
addTerm ps s@(T' _) = PS (tail $ tokens ps) ((Leaf s (head $ tokens ps)): (nodes ps))

addNodesFromTo :: PS -> PS -> PS
addNodesFromTo x y = PS (tokens y) ((nodes y) ++ (nodes x))

completeProd :: Sym -> PS -> PS
completeProd s ps = PS (tokens ps) [Node s (reverse $ nodes ps)]

isTokenTerm :: Lex.Token -> Sym -> Bool
isTokenTerm (Lex.Token ttype tdata) (T stype val) =
  (ttype == stype) && ((map toLower $ Lex.value tdata) == val)
isTokenTerm (Lex.Token ttype _) (T' stype) = ttype == stype

tryProd :: PS -> [Sym] -> [PS]
tryProd ps [] = [ps]
-- Why do I have to repeat this?
tryProd ps (sh@(T' _):ss)
  | null $ tokens ps           = [PS [] []]
  | isTokenTerm (head $ tokens ps) sh = tryProd (addTerm ps sh) ss
  | otherwise = [PS (tokens ps) []] -- Do I need to re-get the tokens?
tryProd ps (sh@(T _ _):ss)
  | null $ tokens ps           = [PS [] []]
  | isTokenTerm (head $ tokens ps) sh = tryProd (addTerm ps sh) ss
  | otherwise = [PS (tokens ps) []]
tryProd ps (sh:ss) =
  let attempt = map (addNodesFromTo ps) (parseSym (PS (tokens ps) []) sh)
  in  filter isValidPS $ concatMap (\x -> tryProd x ss) attempt

-- TODO combine with parseSym
trySym :: PS -> [[Sym]] -> [PS]
trySym tokens prods = filter isValidPS $ concatMap (tryProd tokens) prods

parseSym :: PS -> Sym -> [PS]
parseSym ps sym = map (completeProd sym) (trySym ps (getProds sym))

filterParseable :: [Lex.Token] -> [Lex.Token]
filterParseable tokens = 
  let shouldKeep (Lex.Token Lex.Whitespace _) = False
      shouldKeep _ = True
  in filter shouldKeep tokens

--parse :: [Lex.Token] -> [PS]
parse lexTokens = do 
  let allStates = parseSym (PS  (filterParseable lexTokens) []) S
  --length $ nodes $ head $ filter (null . tokens) allStates
  filter (null . tokens) allStates
