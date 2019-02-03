module AST (
  Node (..),
  --Sym (..)
  buildAST
) where

import qualified Grammar as G 
import qualified Lex as Lex

data Node = Node Sym [Node] |
  Leaf Value |
  Wrapper G.Sym [Node] |
  ValWrapper G.Node

data DType = IntType | FloatType | ProcType
  deriving (Show, Eq)

--data Value = FloatVal Double | IntVal Int | NameVal String
data Value = IntVal Int | NameVal DType String

instance Show Value where
  --show (FloatVal x) = "FloatVal" ++ (show x)
  show (IntVal x) = "IntVal " ++ (show x)
  show (NameVal _ s) = "NameVal " ++ s

data Sym = 
  Program |
  LetExpr | ShowExpr | 
  DivExpr | MulExpr | SubExpr | AddExpr
    deriving (Show)

instance Show Node where
  show (Leaf val) = "("++(show val)++")"
  show (Node s ns) = "("++(show s)++" "++(concatMap show ns)++")"
  show (Wrapper s ns) = "(G."++(show s)++" "++(concatMap show ns)++")"
  show (ValWrapper gn) = "~" ++ (show gn)


isLet (G.Leaf (G.T _ val) _) = val == "let"
isShow (G.Leaf (G.T _ val) _) = val == "show"

makeLeaf :: G.Node -> Node
makeLeaf (G.Leaf _ (Lex.Token Lex.Name td)) =
  Leaf $ NameVal dtype val
    where val = (Lex.value td)
          dtype = case val of
            'i':_ -> IntType
            'f':_ -> FloatType
            'p':_ -> ProcType
          
makeLeaf (G.Leaf _ (Lex.Token Lex.NumLit td)) =
  Leaf $ IntVal $ (read $ Lex.value td :: Int)
makeLeaf _ = Leaf (IntVal 32)

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

applyProd (G.Node G.Expr (gl@(G.Leaf _ _):gls))
  | isLet gl = [Node LetExpr (concatMap applyProd [head gls, gls !! 2])] -- It feels absolutely disgusting to write this line
  | isShow gl = [Node ShowExpr (concatMap applyProd gls)]
applyProd (G.Node G.Expr ((G.Node G.Val val):[])) = [makeLeaf $ head val]
applyProd (G.Node G.Expr ns@(n0:(G.Node G.BinOp op):n1:[])) =
  [Node (toBinExpr $ head op) (concatMap applyProd [n0, n1])]

applyProd (G.Node G.Val val) = [makeLeaf $ head val]

applyProd gl@(G.Leaf (G.T' Lex.Name) _) = [makeLeaf gl]

--applyProd (G.Node gs ns) = [Wrapper gs (concatMap applyProd ns)]
--applyProd n@(G.Leaf _ _) = [ValWrapper n]
applyProd _ = undefined

buildAST :: G.Node -> Node
buildAST root = head $ applyProd root
