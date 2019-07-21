module AST (
  Node (..),
  Sym (..),
  Value (..),
  DType (..),
  buildAST
) where

import qualified Grammar as G 
import qualified Lex as Lex

import Debug.Trace

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

--data Node = Node Sym [Node] |
data Node = Node { sym :: Sym, children :: [Node] } |
  Leaf { value :: Value } |
  Wrapper G.Sym [Node] |
  ValWrapper G.Node

data DType = IntType | FloatType | ProcType
  deriving (Show, Eq)

--data Value = FloatVal Double | IntVal Int | NameVal String
data Value =
  IntVal Int |
  IntExpr Int |
  NameVal DType String |
  NameExpr DType String |
  OrderVal Ordering -- ?
  -- It seems to me like we need just a plain-old `Name`, but I will let that
  -- slide for now.
  -- Would it be better to have a ProcName newtype for String instead of a
  -- DType? 

instance Show Value where
  --show (FloatVal x) = "FloatVal" ++ (show x)
  show (IntVal x) = "IntVal " ++ (show x)
  show (IntExpr x) = "IntExpr " ++ (show x)
  show (NameVal _ s) = "NameVal " ++ s
  show (NameExpr _ s) = "NameExpr " ++ s

data Sym = Program | 
  LetExpr | ShowExpr | 
  DivExpr | MulExpr | SubExpr | AddExpr |
  ProcCall | Def | ProcDef |
  ArgAssign | ArgList |
  Cond | CondList
    deriving (Show, Eq)

instance Show Node where
  show (Leaf val) = "("++(show val)++")"
  show (Node s ns) = "("++(show s)++" "++(concatMap show ns)++")"
  show (Wrapper s ns) = "(G."++(show s)++" "++(concatMap show ns)++")"
  show (ValWrapper gn) = "~" ++ (show gn)


isLet (G.Leaf (G.T _ val) _) = val == "let"
isShow (G.Leaf (G.T _ val) _) = val == "show"

makeLeaf :: G.Node -> Bool -> Node
makeLeaf (G.Leaf _ (Lex.Token Lex.Name td)) isExpr =
  Leaf $ nameType dtype val
    where nameType = if' isExpr NameExpr NameVal
          val = (Lex.value td)
          dtype = case val of
            'i':_ -> IntType
            'f':_ -> FloatType
            'p':_ -> ProcType

makeLeaf (G.Leaf _ (Lex.Token Lex.NumLit td)) isExpr =
  Leaf $ dtype $ (read $ Lex.value td :: Int)
    where dtype = if' isExpr IntExpr IntVal

makeLeaf x _ = error $ "Could not make leaf for " ++ (show x)

toBinExpr :: G.Node -> Sym
toBinExpr (G.Leaf _ (Lex.Token _ td)) 
  | (Lex.value td) == "/" = DivExpr
  | (Lex.value td) == "*" = MulExpr
  | (Lex.value td) == "-" = SubExpr
  | (Lex.value td) == "+" = AddExpr

applyProd :: G.Node -> [Node]

applyProd (G.Node G.S ns) = [Node Program (applyProd $ head ns)]
applyProd (G.Node G.StL (n0:n1:[])) = (head (applyProd n0)) : (applyProd n1)
applyProd (G.Node G.StL (n0:[])) = applyProd n0
applyProd (G.Node G.St (n0:_)) = applyProd n0

applyProd (G.Node G.ArgList (n0:_:n1:[])) =
  (head (applyProd n0)) : (applyProd n1)
applyProd (G.Node G.ArgList (n0:[])) = applyProd n0
applyProd (G.Node G.ArgAssign (name:_:arg:[])) =
  [Node ArgAssign $ concatMap applyProd [name, arg]]

-- TODO proc "like" is not enforced
applyProd (G.Node G.CondList (n0:_:n1:[])) =
  (head (applyProd n0)) : (applyProd n1)
applyProd (G.Node G.CondList (n0:[])) = applyProd n0
applyProd (G.Node G.Cond (name:[])) =
  [Node Cond $ applyProd name]
applyProd (G.Node G.Cond (name:_:arg:[])) =
  [Node Cond $ concatMap applyProd [name, arg]]

applyProd (G.Node G.Expr (gl@(G.Leaf _ _):gls))
  | isLet gl = [Node LetExpr (concatMap applyProd [head gls, gls !! 2])] -- It feels absolutely disgusting to write this line
  | isShow gl = [Node ShowExpr (concatMap applyProd gls)]
applyProd (G.Node G.Expr ns@(n0:(G.Node G.BinOp op):n1:[])) =
  [Node (toBinExpr $ head op) (concatMap applyProd [n0, n1])]
--applyProd (G.Node G.Expr ((G.Node G.Val val):[])) = [makeLeaf $ head val]
applyProd (G.Node G.Expr ((G.Node G.Val val):[])) = [makeLeaf (head val) True]

applyProd (G.Node G.Expr (n@(G.Node G.ProcCall def):[])) = applyProd n
--applyProd (G.Node G.Expr ns) = applyProd ns

applyProd (G.Node G.Val val) = [makeLeaf (head val) False]

applyProd gl@(G.Leaf (G.T' Lex.Name) _) = [makeLeaf gl False]

applyProd (G.Node G.ProcDef (name:_:def:[])) =
  [Node ProcDef ((makeLeaf name False) : (applyProd def))]
applyProd (G.Node G.ProcDef (_:conds:_:name:_:def:[])) =
  let children = (Node CondList (applyProd conds)) : ((makeLeaf name False) : (applyProd def))
  in [Node ProcDef children]
applyProd (G.Node G.ProcCall (_:def:[])) =
  [Node ProcCall (applyProd def)]
-- Should I use indexing instead of PM? Or just use arrays?
applyProd (G.Node G.ProcCall (_:al:rest)) = do
  let argList = Node ArgList $ applyProd al
  [Node ProcCall $ argList : (concatMap applyProd rest)]
applyProd (G.Node G.Def (name:[])) = [makeLeaf name False]
applyProd (G.Node G.Def (_:stl:_)) = [Node Def (applyProd stl)]

-- Discard random keywords and symbols by default
applyProd (G.Leaf _ _) = []

--applyProd (G.Node gs ns) = [Wrapper gs (concatMap applyProd ns)]
--applyProd n@(G.Leaf _ _) = [ValWrapper n]
applyProd x = error $ "No AST production for " ++ show x

buildAST :: G.Node -> Node
buildAST root = head $ applyProd root
