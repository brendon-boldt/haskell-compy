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

-- Make EOFError better; add syms eventually
data Error = Error Lex.Token | NoError | EOFError
  deriving (Show)

instance Eq Error where
  NoError == NoError = True
  EOFError == EOFError = True
  (Error (Lex.Token _ x)) == (Error (Lex.Token _ y)) =
    (Lex.loc x) == (Lex.loc y)
  _ == _ = False

instance Ord Error where
  compare (Error (Lex.Token _ x)) (Error (Lex.Token _ y)) =
    compare (Lex.loc x) (Lex.loc y)
  compare NoError NoError = EQ
  compare NoError _ = LT
  compare _ NoError = GT
  compare EOFError EOFError = EQ
  compare _ EOFError = LT
  compare EOFError _ = GT

--data Error =
--  TokenError Sym Lex.Token |
--  EOFError Sym |
--  SymbolError Sym Error
--  --SymbolError Sym Lex.Token
--    deriving (Show)

-- Parser State
data PS = PS { tokens :: [Lex.Token],
               nodes :: [Node],
               error :: Error }
  deriving (Show)

--type LKW = Lex.Keyword
lKW = Lex.Keyword

-- TODO use a (hash)map
getProds :: Sym -> [[Sym]]
getProds S = [[StL, T' Lex.EOF]]
getProds StL = [[St, StL],
                [St]]
getProds St = [[Expr, T lKW "."]]
getProds Expr = [[T lKW "let", T' Lex.Name, T lKW "be", Expr],
                 [T lKW "show", Expr],
                 [T' Lex.NumLit],
                 [T' Lex.Name]]

isNoError :: Error -> Bool
isNoError NoError = True
isNoError _ = False

isValidPS :: PS -> Bool
isValidPS ps = isNoError $ Parse.error ps

-- TODO make typeclass instance
compareTokens :: Lex.Token -> Lex.Token -> Ordering
compareTokens (Lex.Token _ tdx) (Lex.Token _ tdy) =
  compare (Lex.loc tdx) (Lex.loc tdy)

updateError :: PS -> Error -> PS
updateError ps e@(Error eTok) = case (Parse.error ps) of
  NoError -> PS (tokens ps) (nodes ps) e
  EOFError -> ps
  (Error t) -> case (compareTokens t eTok) of 
    --EQ -> PS (tokens ps) (nodes ps) (Error (eSyms ++ syms) t)
    EQ -> ps
    GT -> ps
    LT -> PS (tokens ps) (nodes ps) e

addTerm :: PS -> Sym -> PS
addTerm ps s@(T _ _) = PS (tail $ tokens ps) ((Leaf s (head $ tokens ps)): (nodes ps)) (Parse.error ps)
addTerm ps s@(T' _) = PS (tail $ tokens ps) ((Leaf s (head $ tokens ps)): (nodes ps)) (Parse.error ps)

addNodesFromTo :: PS -> PS -> PS
--addNodesFromTo x y = PS (tokens y) ((nodes y) ++ (nodes x)) []
addNodesFromTo x y = PS (tokens y) ((nodes y) ++ (nodes x)) (Parse.error y) 

completeProd :: Sym -> PS -> PS
--completeProd s ps = PS (tokens ps) [Node s (reverse $ nodes ps)] (trace (show (Parse.error ps)) (Parse.error ps))
completeProd s ps = if isValidPS ps
  then PS (tokens ps) [Node s (reverse $ nodes ps)] NoError
  --else PS (tokens ps) [] (Parse.error (trace (show $ Parse.error ps) ps))
  else PS (tokens ps) [] (Parse.error ps)

isTokenTerm :: Lex.Token -> Sym -> Bool
isTokenTerm (Lex.Token ttype tdata) (T stype val) =
  (ttype == stype) && ((map toLower $ Lex.value tdata) == val)
isTokenTerm (Lex.Token ttype _) (T' stype) = ttype == stype

tryProd :: PS -> [Sym] -> [PS]
-- Not sure about this line
--tryProd ps _ | not $ isValidPS ps = [PS (tokens ps) [] (Error (head $ tokens ps))]
tryProd ps _ | not $ isValidPS ps = [ps]
tryProd ps [] = [ps]
-- Why do I have to repeat this?
tryProd ps (sh@(T' _):ss)
  | null $ tokens ps           = [PS [] [] EOFError]
  | isTokenTerm (head $ tokens ps) sh = tryProd (addTerm ps sh) ss
  -- | otherwise = [PS (tokens ps) [] [TokenError sh (head $ tokens ps)]] -- Do I need to re-get the tokens?
  | otherwise = [updateError ps (Error (head $ tokens ps))] -- Do I need to re-get the tokens?
tryProd ps (sh@(T _ _):ss)
  | null $ tokens ps           = [PS [] [] EOFError]
  | isTokenTerm (head $ tokens ps) sh = tryProd (addTerm ps sh) ss
  | otherwise = [updateError ps (Error (head $ tokens ps))]
tryProd ps (sh:ss) = do
  let attempts = map (addNodesFromTo ps) (parseSym (PS (tokens ps) [] NoError) sh)
  let filtered = filter isValidPS attempts
  let maxError = (maximum $ map Parse.error attempts)
  if null filtered
    then [PS (tokens ps) [] maxError]
    else concatMap (\x -> tryProd x ss) filtered

-- TODO combine with parseSym
trySym :: PS -> [[Sym]] -> [PS]
--trySym tokens prods = filter isValidPS $ concatMap (tryProd tokens) prods
--trySym ps prods = filter isValidPS $ concatMap (tryProd ps) prods
trySym ps prods = concatMap (tryProd ps) prods

parseSym :: PS -> Sym -> [PS]
--parseSym ps sym | not $ isValidPS ps = [PS (tokens ps) [] (Error (head $ tokens ps))]
parseSym ps sym | not $ isValidPS ps = [ps]
parseSym ps sym = do
  let attempts = map (completeProd sym) (trySym ps (getProds sym))
  let filtered = filter isValidPS attempts
  let maxError = (maximum $ map Parse.error attempts)
  if null filtered
    then [PS (tokens ps) [] maxError]
    else (trace (show (sym,  attempts)) filtered)

filterParseable :: [Lex.Token] -> [Lex.Token]
filterParseable tokens = 
  let shouldKeep (Lex.Token Lex.Whitespace _) = False
      shouldKeep _ = True
  in filter shouldKeep tokens

--parse :: [Lex.Token] -> [PS]
parse lexTokens = do 
  let allStates = parseSym (PS  (filterParseable lexTokens) [] NoError) S
  --length $ nodes $ head $ filter (null . tokens) allStates
  if any isValidPS allStates
    then filter (null . tokens) allStates
    else allStates
