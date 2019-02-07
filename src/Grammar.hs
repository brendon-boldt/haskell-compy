module Grammar (
  Sym (..),
  Node (..),
  getProds
) where

import qualified Lex as Lex

data Sym = S | StL | St | Expr | Val | 
  ProcCall | ProcDef | Def | Cond | BinOp |
  T Lex.TType String |
  T' Lex.TType
  deriving (Show)

{- Grammar -

S = StL EOF
StL = St StL
    | St
St = Expr "."
   | ProcDef "."
Expr = "let" Name "be" Expr
     | "show" Expr
     | Val
     | Val "+" Expr
     | Val "-" Expr
     | ProcCall
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
                 --[Val, lS "+", Expr],
                 [Val, BinOp, Expr],
                 [Val]]
getProds BinOp = [[lS "+"], [lS "-"], [lS "*"], [lS "/"]]
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