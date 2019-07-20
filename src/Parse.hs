module Parse (
  --Sym (..),
  --Node (..),
  parse,
  PS (nodes),
  showPS
) where

import Debug.Trace
import Data.Maybe
import Data.Char (toLower)

import qualified Lex as Lex
import qualified Grammar as G

-- TODO try ambiguous grammar

data Error = Error | NoError
  deriving (Show, Eq)

instance Ord Lex.Token where
  compare (Lex.Token _ x) (Lex.Token _ y) = compare (Lex.loc x) (Lex.loc y)

instance Eq Lex.Token where
  (Lex.Token _ x) == (Lex.Token _ y) = (Lex.loc x) == (Lex.loc y)

-- Parser State
data PS = PS { tokens :: [Lex.Token]
             , nodes :: [G.Node]
             , error :: Error
             , lastToken :: Lex.Token}
  deriving (Show)


isNoError :: Error -> Bool
isNoError NoError = True
isNoError _ = False

isValidPS :: PS -> Bool
isValidPS ps = isNoError $ Parse.error ps

updateLastToken :: PS -> Lex.Token -> PS
updateLastToken ps t = 
  if ((lastToken ps) > t)
    then ps
    --else PS (tokens ps) (nodes ps) (Parse.error ps) t
    else ps { lastToken = t }

setError :: PS -> Lex.Token -> PS
setError ps t = 
  --let newPS = PS (tokens ps) (nodes ps) Error 
  --let newPS = ps { Parse.error = Error }
  --in if (lastToken ps) > t
  if (lastToken ps) > t
    then ps { Parse.error = Error }
    else ps { Parse.error = Error, lastToken = t }


addTerm :: PS -> G.Sym -> PS
addTerm ps s@(G.T _ _) = addTerm' ps s
addTerm ps s@(G.T' _) = addTerm' ps s
--addTerm' ps s = PS (tail $ tokens ps) ((G.Leaf s (head $ tokens ps)): (nodes ps)) (Parse.error ps) (lastToken ps)
addTerm' ps s = ps { tokens = (tail $ tokens ps)
                   , nodes = ((G.Leaf s (head $ tokens ps)): (nodes ps)) }

addNodesFromTo :: PS -> PS -> PS
--addNodesFromTo x y = PS (tokens y) ((nodes y) ++ (nodes x)) (Parse.error y) (lastToken y)
addNodesFromTo x y = y { nodes = (nodes y) ++ (nodes x) }

completeProd :: G.Sym -> PS -> PS
completeProd s ps = if isValidPS ps
  --then PS (tokens ps) [G.Node s (reverse $ nodes ps)] NoError (lastToken ps)
  --else PS (tokens ps) [] (Parse.error ps) (lastToken ps)
  then ps { nodes = [G.Node s (reverse $ nodes ps)], Parse.error = NoError }
  else ps { nodes = [] } 

isTokenTerm :: Lex.Token -> G.Sym -> Bool
isTokenTerm (Lex.Token ttype tdata) (G.T stype val) =
  (ttype == stype) && ((map toLower $ Lex.value tdata) == val)
isTokenTerm (Lex.Token ttype _) (G.T' stype) = ttype == stype

isEOFToken :: Lex.Token -> Bool
isEOFToken (Lex.Token Lex.EOF _) = True
isEOFToken _ = False

tryProd :: PS -> [G.Sym] -> [PS]
tryProd ps _ | not $ isValidPS ps = [ps]
tryProd ps [] = [ps]
-- Why do I have to repeat this?
tryProd ps (sh@(G.T' _):ss)
  | isTokenTerm (head $ tokens ps) sh = tryProd (addTerm ps sh) ss
  | isEOFToken (head $ tokens ps) = [PS [] [] Error (head $ tokens ps)]
  | otherwise = [setError ps (head $ tokens ps)]
tryProd ps (sh@(G.T _ _):ss)
  | isTokenTerm (head $ tokens ps) sh = tryProd (addTerm ps sh) ss
  | isEOFToken (head $ tokens ps) = [PS [] [] Error (head $ tokens ps)]
  | otherwise = [setError ps (head $ tokens ps)]
tryProd ps (sh:ss) = do
  let attempts = map (addNodesFromTo ps) (parseSym (PS (tokens ps) [] NoError (head $ tokens ps)) sh)
  let filtered = filter isValidPS attempts
  let maxToken = (maximum $ map lastToken attempts)
  if null filtered
    then [PS (tokens ps) [] Error maxToken]
    else concatMap (\x -> tryProd x ss) filtered

-- TODO combine with parseSym
trySym :: PS -> [[G.Sym]] -> [PS]
trySym ps prods = concatMap (tryProd ps) prods

parseSym :: PS -> G.Sym -> [PS]
parseSym ps sym | not $ isValidPS ps = [ps]
parseSym ps sym = do
  let attempts = map (completeProd sym) (trySym ps (G.getProds sym))
  let maxToken = (maximum $ map lastToken attempts)
  let filtered =  filter isValidPS attempts
  let updated = map (\x -> updateLastToken x maxToken) filtered
  if null filtered
    then [PS (tokens ps) [] Error maxToken]
    -- Using `updated` instead of `filtered` makes things a lot slower due to
    -- the growing parse tree when we have long files.
    else updated
    --else filtered

filterParseable :: [Lex.Token] -> [Lex.Token]
filterParseable tokens = 
  let shouldKeep (Lex.Token Lex.Whitespace _) = False
      shouldKeep (Lex.Token Lex.Comment _) = False
      shouldKeep _ = True
  in filter shouldKeep tokens

parse :: [Lex.Token] -> [PS]
parse lexTokens = do 
  let parseable = filterParseable lexTokens
  let allStates = parseSym (PS parseable [] NoError (head parseable)) G.S
  if any isValidPS allStates
    then filter (null . tokens) allStates
    else allStates

showPS :: PS -> String
showPS ps | Parse.error ps == Error = 
  Prelude.error $ "Parse failed at " ++ (show $ lastToken ps)
showPS ps = show $ head $ nodes ps
