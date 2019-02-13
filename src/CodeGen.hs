module CodeGen
  ( generateAsm
  ) where

--import Data.HashMap.Strict
import Data.Maybe (isJust, fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad
import Control.Monad.State
import Control.Applicative (liftA2)

import Debug.Trace

import qualified AST as A
import qualified Asm as Asm

-- Eventually we will have to keep track of the type of the vars
data CGState = CGState { vars :: Map.Map String Int
                       , sp :: Int }
                       deriving (Show)

initCGState = CGState Map.empty (-1)

-- CodeGen Monad
type CGM = State CGState

--instance Show CGM where
  --show State cgs = "CGM (" ++ (show cgs) ++ ")"
  
cgmPlus :: CGM [a] -> CGM [a] -> CGM [a] 
cgmPlus x y = liftA2 (++) x y

getOffset :: CGState -> String -> Maybe Int
getOffset cgs var =
  let calc = (\x -> (x - (sp cgs)) * 8)
  in calc <$> (Map.lookup var (vars cgs))

getStackSpace :: CGState -> Int
getStackSpace cgs = (sp cgs + 1) * 8

newVar :: String -> CGM ()
newVar name = state (\cgs ->
  let newSP = sp cgs + 1
      newVarMap = Map.insert name newSP (vars cgs)
  in ((), CGState newVarMap newSP))

handleLet :: A.Node -> CGM [T.Text]
handleLet (A.Leaf (A.NameVal _ name)) = do
  cgs <- get
  let maybeOffset = getOffset cgs name
  if isJust maybeOffset
    then return $ Asm.exprToStack (fromJust maybeOffset)
    else newVar name >> (return $ Asm.newVar ++ (Asm.exprToStack 0))
    -- 0 because we always know where a new variable is going -- maybe this
    -- will change at some point?

handleNameExpr :: String -> CGM [T.Text]
--handleNameExpr (A.Leaf (A.NameExpr _ name)) = do
handleNameExpr name = do
  cgs <- get
  let maybeOffset = getOffset cgs name
  if isJust maybeOffset
    then return $ Asm.movVarRax (fromJust maybeOffset)
    else undefined

-- TODO Should I build everything in reverse?
build :: A.Node -> CGM [T.Text]

-- Since Program is effectively a procedure, we need to clean up the stack
-- before we exit.
build (A.Node A.Program cs) = do
  -- TODO intercalate newlines or something
  body <- foldr1 cgmPlus $ map build cs
  cgs <- get
  return $ body ++ (Asm.adjustRsp (getStackSpace cgs))
build (A.Node A.ShowExpr (c:[])) = (build c) `cgmPlus` (return [Asm.showAsm])
-- Refactor this into something nicer when I feel like it
build (A.Node A.AddExpr (val:expr:[])) =
  (build expr) `cgmPlus` (return $ Asm.addVal val)
build (A.Node A.SubExpr (val:expr:[])) =
  (build expr) `cgmPlus` (return $ Asm.subVal val)
build (A.Node A.MulExpr (val:expr:[])) =
  (build expr) `cgmPlus` (return $ Asm.mulVal val)
build (A.Leaf (A.IntExpr val)) = return $ Asm.movValRax val
--build (A.Node A.LetExpr (n:e:[])) = liftA2 (++) (build e) (handleLet n)
build (A.Node A.LetExpr (n:e:[])) = (build e) `cgmPlus` (handleLet n)
build (A.Leaf (A.NameExpr _ name)) = handleNameExpr name

build x = (trace (show x) undefined)

-- Monad m => m [a] -> m [a] -> m [a]

generateAsm :: A.Node -> IO ()
generateAsm ast = do
  let fn = "asm/main.s"
  let program = evalState (build ast) initCGState
  mapM_ (TIO.appendFile fn) (Asm.preamble : program)
  TIO.appendFile fn Asm.postlude
