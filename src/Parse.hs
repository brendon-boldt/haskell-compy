module Parse (
  Sym (..),
  Node (..),
  parse,
  showPS
) where

import Debug.Trace
import Data.Maybe
import Data.Char (toLower)

import qualified Lex as Lex

data Sym = S | StL | St | Expr | Nm | NumLit | Val | 
  ProcCall | ProcDef | Def | Cond |
  T Lex.TType String |
  T' Lex.TType
  deriving (Show)


{- Grammar -

S = StL EOF
StL = St StL
    | St
St = Expr "."
   | ProcDef "."
   | ProcCall "."
Expr = "let" Name "be" Expr
     | "show" Expr
     | Val
     | Val "+" Expr
     | Val "-" Expr
Val = Name | NumLit

ProcDef = "when" Cond Name "does" Def
        | Name "does" Def
Def = "`" StL "'"
    | Name
Cond = Expr "is" Expr
     | Expr "is" "greater" "than" Expr
     | Expr "is" "less" "than" Expr

ProcCall = "given" Name "is" Expr "do" Def
         | "do" Def

-  End Grammar -}

--type LKW = Lex.Keyword
lKW = T Lex.Keyword
lS = T Lex.Symbol

-- TODO use a (hash)map
getProds :: Sym -> [[Sym]]
getProds S = [[StL, T' Lex.EOF]]
getProds StL = [[St, StL],
                [St]]
getProds St = [[Expr, lS "."],
               [ProcDef, lS "."],
               [ProcCall, lS "."]]
getProds Expr = [[lKW "let", T' Lex.Name, lKW "be", Expr],
                 [lKW "show", Expr],
                 [Val, lS "+", Expr],
                 [Val, lS "-", Expr],
                 [Val, lS "*", Expr],
                 [Val, lS "/", Expr],
                 [Val]]
getProds Val = [[T' Lex.Name],
                [T' Lex.NumLit]]
getProds ProcDef = [[lKW "when", Cond, lS ",", T' Lex.Name, lKW "does", Def],
                    [T' Lex.Name, lKW "does", Def]]
getProds Def = [[lS "`", StL, lS "'"],
                [T' Lex.Name]]
getProds Cond = [[Expr, lKW "is", Expr],
                 [Expr, lKW "is", lKW "less", lKW "than", Expr],
                 [Expr, lKW "is", lKW "greater", lKW "than", Expr]]
getProds ProcCall =
  [[lKW "given", T' Lex.Name, lKW "is", Expr, lS ",", lKW "do", Def],
   [lKW "do",  Def]]

data Node = Node Sym [Node] | Leaf Sym Lex.Token

instance Show Node where
  show (Leaf (T ttype _) (Lex.Token _ td)) =
    "("++(show ttype)++" "++(Lex.value td)++")"
  show (Leaf (T' ttype) (Lex.Token _ td)) =
    "("++(show ttype)++" "++(Lex.value td)++")"
  show (Node s ns) = "("++(show s)++" "++(concatMap show ns)++")"

data Error = Error | NoError
  deriving (Show, Eq)

instance Ord Lex.Token where
  compare (Lex.Token _ x) (Lex.Token _ y) = compare (Lex.loc x) (Lex.loc y)

instance Eq Lex.Token where
  (Lex.Token _ x) == (Lex.Token _ y) = (Lex.loc x) == (Lex.loc y)
  

-- Parser State
data PS = PS { tokens :: [Lex.Token],
               nodes :: [Node],
               error :: Error, 
               lastToken :: Lex.Token}
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
    else PS (tokens ps) (nodes ps) (Parse.error ps) t

setError :: PS -> Lex.Token -> PS
setError ps t = 
  let newPS = PS (tokens ps) (nodes ps) Error 
  in if (lastToken ps) > t
    then newPS (lastToken ps)
    else newPS t

addTerm :: PS -> Sym -> PS
addTerm ps s@(T _ _) = PS (tail $ tokens ps) ((Leaf s (head $ tokens ps)): (nodes ps)) (Parse.error ps) (lastToken ps)
addTerm ps s@(T' _) = PS (tail $ tokens ps) ((Leaf s (head $ tokens ps)): (nodes ps)) (Parse.error ps) (lastToken ps)

addNodesFromTo :: PS -> PS -> PS
addNodesFromTo x y = PS (tokens y) ((nodes y) ++ (nodes x)) (Parse.error y) (lastToken y)

completeProd :: Sym -> PS -> PS
completeProd s ps = if isValidPS ps
  then PS (tokens ps) [Node s (reverse $ nodes ps)] NoError (lastToken ps)
  else PS (tokens ps) [] (Parse.error ps) (lastToken ps)

isTokenTerm :: Lex.Token -> Sym -> Bool
isTokenTerm (Lex.Token ttype tdata) (T stype val) =
  (ttype == stype) && ((map toLower $ Lex.value tdata) == val)
isTokenTerm (Lex.Token ttype _) (T' stype) = ttype == stype

isEOFToken :: Lex.Token -> Bool
isEOFToken (Lex.Token Lex.EOF _) = True
isEOFToken _ = False

tryProd :: PS -> [Sym] -> [PS]
tryProd ps _ | not $ isValidPS ps = [ps]
tryProd ps [] = [ps]
-- Why do I have to repeat this?
tryProd ps (sh@(T' _):ss)
  | isTokenTerm (head $ tokens ps) sh = tryProd (addTerm ps sh) ss
  | isEOFToken (head $ tokens ps) = [PS [] [] Error (head $ tokens ps)]
  | otherwise = [setError ps (head $ tokens ps)]
tryProd ps (sh@(T _ _):ss)
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
trySym :: PS -> [[Sym]] -> [PS]
trySym ps prods = concatMap (tryProd ps) prods

parseSym :: PS -> Sym -> [PS]
parseSym ps sym | not $ isValidPS ps = [ps]
parseSym ps sym = do
  let attempts = map (completeProd sym) (trySym ps (getProds sym))
  let maxToken = (maximum $ map lastToken attempts)
  let filtered =  filter isValidPS attempts
  let updated = map (\x -> updateLastToken x maxToken) filtered
  if null filtered
    then [PS (tokens ps) [] Error maxToken]
    else updated

filterParseable :: [Lex.Token] -> [Lex.Token]
filterParseable tokens = 
  let shouldKeep (Lex.Token Lex.Whitespace _) = False
      shouldKeep (Lex.Token Lex.Comment _) = False
      shouldKeep _ = True
  in filter shouldKeep tokens

parse :: [Lex.Token] -> [PS]
parse lexTokens = do 
  let parseable = filterParseable lexTokens
  let allStates = parseSym (PS parseable [] NoError (head parseable)) S
  if any isValidPS allStates
    then filter (null . tokens) allStates
    else allStates

showPS :: PS -> String
showPS ps | Parse.error ps == Error = 
  "Parse failed at " ++ (show $ lastToken ps)
showPS ps = show $ head $ nodes ps
