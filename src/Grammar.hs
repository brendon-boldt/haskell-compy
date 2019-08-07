module Grammar (
  Sym (..),
  Node (..),
  getProds
) where

import qualified Lex as Lex

data Sym = S | StL | St | Expr | Val | 
  ProcCall | ProcDef | Def | Cond | BinOp |
  Arg | ArgAssign | ArgList | 
  CondList |
  T Lex.TType String |
  T' Lex.TType
  deriving (Show)

--type LKW = Lex.Keyword
lKW = T Lex.Keyword
lS = T Lex.Symbol

-- TODO use a (hash)map
getProds :: Sym -> [[Sym]]
getProds S = [[StL, T' Lex.EOF]]
getProds StL = [[St, StL],
                [St]]
getProds St = [ [Expr, lS "."]
              , [ProcDef, lS "."] ]
               --[ProcCall, lS "."]]
getProds Expr = [ [lKW "let", T' Lex.Name, lKW "be", Expr]
                , [lKW "show", Expr]
                , [ProcCall]
                , [Val, BinOp, Expr]
                , [Val] ]
getProds BinOp = [[lS "+"], [lS "-"], [lS "*"], [lS "/"]]
getProds Val = [ [T' Lex.Name]
               , [T' Lex.NumLit] ]
               --, [ProcCall] ] -- This was commented out, and I don't know why.
getProds ProcDef = [[lKW "with", CondList, lS ",", T' Lex.Name, lKW "does", Def],
                    [T' Lex.Name, lKW "does", Def]]
getProds Def = [ [lS "`", StL, lS "'"]
               , [T' Lex.Name] ]

getProds CondList = [ [Cond, lKW "and", CondList]
                    , [Cond] ]
getProds Cond = [ [T' Lex.Name]
                , [T' Lex.Name, lKW "like", T' Lex.Name]
                , [T' Lex.Name, lKW "being", Expr]
                , [T' Lex.Name, lKW "less", lKW "than", Expr]
                , [T' Lex.Name, lKW "greater", lKW "than", Expr] ]
                -- , [T' Lex.Name, lKW "being", lKW "less", lKW "than", Expr]
                -- , [T' Lex.Name, lKW "being", lKW "greater", lKW "than", Expr] ]

--getProds Arg = [ [Def], [Expr] ]
getProds ArgAssign = [ [T' Lex.Name, lKW "is", Expr]
                     , [T' Lex.Name, lKW "does", Def]
                     , [T' Lex.Name] ]
getProds ArgList = [ [ArgAssign]
                   , [ArgAssign, lKW "and", ArgList] ]
getProds ProcCall =
  [[lKW "given", ArgList, lS ",", lKW "do", Def],
   [lKW "do",  Def]]
  --[[lKW "given", T' Lex.Name, lKW "is", Expr, lS ",", lKW "do", Def],
  -- [lKW "do",  Def]]

data Node = Node Sym [Node] | Leaf Sym Lex.Token

instance Show Node where
  show (Leaf (T ttype _) (Lex.Token _ td)) =
    "("++(show ttype)++" "++(Lex.value td)++")"
  show (Leaf (T' ttype) (Lex.Token _ td)) =
    "("++(show ttype)++" "++(Lex.value td)++")"
  show (Node s ns) = "("++(show s)++" "++(concatMap show ns)++")"
