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
import Control.Monad.State

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

data AstState = AstState { tailSt :: Bool }

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
  show (OrderVal o) = "OrderVal " ++ (show o)

data Sym = Program | 
  LetExpr | ShowExpr | 
  DivExpr | MulExpr | SubExpr | AddExpr |
  ProcCall | ProcTailCall |
  Def | ProcDef |
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

setTailSt :: Bool -> State AstState ()
setTailSt b =
  state $ \s -> ((), s { tailSt = b })

getProcCallConstructor :: State AstState Sym
getProcCallConstructor = do
  as <- get
  if tailSt as
    then setTailSt False >> return ProcTailCall
    else return ProcCall

--applyProd :: G.Node -> [Node]
applyProd :: G.Node -> State AstState [Node]

applyProd (G.Node G.S ns) =
  pure . (Node Program) <$> (applyProd $ head ns)
applyProd (G.Node G.StL (n0:n1:[])) = do
  stNode <- head <$> applyProd n0
  nodes <- applyProd n1
  return $ stNode : nodes
applyProd (G.Node G.StL (n0:[])) = do
  state (\s -> ((), s { tailSt = True }))
  applyProd n0
applyProd (G.Node G.St (n0:_)) = applyProd n0

applyProd (G.Node G.ArgList (n0:_:n1:[])) = do
  argNode <- head <$> applyProd n0
  nodes <- applyProd n1
  return $ argNode : nodes
applyProd (G.Node G.ArgList (n0:[])) = applyProd n0
applyProd (G.Node G.ArgAssign (name:_:arg:[])) = do
  nodes <- concat <$> (sequence $ map applyProd [name, arg])
  return $ pure $ Node ArgAssign nodes
applyProd (G.Node G.ArgAssign (name:[])) = do
  -- Use name here twice for implciit self-assign
  nodes <- concat <$> (sequence $ map applyProd [name, name])
  return $ pure $ Node ArgAssign $ nodes

-- TODO proc "like" is not enforced
applyProd (G.Node G.CondList ( n0
                             : (G.Leaf (G.T Lex.Keyword "and") _)
                             : n1
                             : [])) = do
  condNode <- head <$> (applyProd n0)
  nodes <- applyProd n1
  return $ condNode:nodes
applyProd (G.Node G.CondList (n0:[])) = applyProd n0
applyProd (G.Node G.Cond (name:[])) =
  pure . (Node Cond) <$> (applyProd name)
applyProd (G.Node G.Cond ( name
                         : order@(G.Leaf (G.T Lex.Keyword "being") _)
                         : arg
                         : [])) = do
  nodes <- fmap concat $ sequence $ map applyProd [name, order, arg]
  return $ pure $ Node Cond nodes
applyProd (G.Node G.Cond ( name
                         : (G.Leaf (G.T Lex.Keyword "like") _)
                         : arg
                         : [])) = do
  nodes <- fmap concat $ sequence $ map applyProd [name, arg]
  return $ pure $ Node Cond nodes
applyProd (G.Node G.Cond ( name
                         -- : (G.Leaf (G.T Lex.Keybword order) _)
                         : order@(G.Leaf _ _)
                         : (G.Leaf (G.T Lex.Keyword "than") _)
                         : arg
                         : [])) = do
  nodes <- fmap concat $ sequence $ map applyProd [name, order, arg]
  return $ pure $ Node Cond nodes

applyProd (G.Node G.Expr (gl@(G.Leaf _ _):gls))
  | isLet gl = do
    glsNode <- fmap concat $ sequence (map applyProd [head gls, gls !! 2])
    return $ pure $ Node LetExpr glsNode
  -- It feels absolutely disgusting to write this line
  | isShow gl = do
    glsNode <- fmap concat $ sequence (map applyProd gls)
    return $ pure $ Node ShowExpr glsNode
applyProd (G.Node G.Expr ns@(n0:(G.Node G.BinOp op):n1:[])) = do
  operands <- fmap concat $ sequence (map applyProd [n0, n1])
  return $ pure $ Node (toBinExpr $ head op) operands
--applyProd (G.Node G.Expr ((G.Node G.Val val):[])) = [makeLeaf $ head val]
applyProd (G.Node G.Expr ((G.Node G.Val val):[])) =
  return $ pure $ makeLeaf (head val) True

applyProd (G.Node G.Expr (n@(G.Node G.ProcCall def):[])) = do
  as <- get
  if tailSt as
    then setTailSt True >> (applyProd n)
    else applyProd n
--applyProd (G.Node G.Expr ns) = applyProd ns

applyProd (G.Node G.Val val) = return $ pure $ makeLeaf (head val) False

applyProd gl@(G.Leaf (G.T' Lex.Name) _) = return $ pure $ makeLeaf gl False

applyProd (G.Node G.ProcDef (name:_:def:[])) = do
  defNode <- applyProd def
  return $ pure $ Node ProcDef ((makeLeaf name False) : defNode)
applyProd (G.Node G.ProcDef (_:conds:_:name:_:def:[])) = do
  defNodes <- (applyProd def)
  condNode <- fmap (Node CondList) (applyProd conds) 
  let children = condNode : ((makeLeaf name False) : defNodes)
  return $ pure $ Node ProcDef children
applyProd (G.Node G.ProcCall (_:def:[])) = do
  pcConstructor <- getProcCallConstructor
  fmap (pure . (Node pcConstructor)) (applyProd def)
-- Should I use indexing instead of PM? Or just use arrays?
applyProd (G.Node G.ProcCall (_:al:rest)) = do
  argList <- fmap (Node ArgList) $ applyProd al
  appliedRest <- fmap concat $ sequence $ map applyProd rest
  pcConstructor <- getProcCallConstructor
  return $ pure $ Node pcConstructor $ argList : appliedRest
applyProd (G.Node G.Def (name:[])) = return [makeLeaf name False]
applyProd (G.Node G.Def (_:stl:_)) =
  liftM (pure . (Node Def)) $ applyProd stl

applyProd (G.Leaf (G.T Lex.Keyword "greater") _) = return [Leaf $ OrderVal GT]
applyProd (G.Leaf (G.T Lex.Keyword "less") _)    = return [Leaf $ OrderVal LT]
applyProd (G.Leaf (G.T Lex.Keyword "being") _)   = return [Leaf $ OrderVal EQ]


-- Discard random keywords and symbols by default
applyProd (G.Leaf _ _) = return []

--applyProd (G.Node gs ns) = [Wrapper gs (concatMap applyProd ns)]
--applyProd n@(G.Leaf _ _) = [ValWrapper n]
applyProd x = error $ "No AST production for " ++ show x

buildAST :: G.Node -> Node
--buildAST root = head $ applyProd root
buildAST root =
 let initState = AstState { tailSt = False }
 in  head $ evalState (applyProd root) initState
