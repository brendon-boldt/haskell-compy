{-# Language OverloadedStrings #-}
module CodeGen
  ( generateAsm
  ) where

--import Data.HashMap.Strict
import Data.Maybe (isJust, fromJust)
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad
--import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.State
import System.IO

import Debug.Trace

import qualified AST as A
import qualified Asm as Asm

data Proc = Proc { procdef :: A.Node
                 , conds :: Maybe A.Node }
-- Make this an instance of foldable later.

data CGState = CGState { vars :: Map.Map String Int
                       , procs :: Map.Map String [Proc]
                       , sp :: Int
                       , cgwrite :: [T.Text] -> IO () }

initCGState :: Handle -> CGState
--initCGState = CGState Map.empty (-1)
initCGState handle = CGState
                     Map.empty
                     Map.empty
                     (-1)
                     (mapM_ (TIO.hPutStr handle))

getOffset :: CGState -> String -> Maybe Int
getOffset cgs var =
  let calc = (\x -> ((sp cgs) - x) * 8)
  in calc <$> (Map.lookup var (vars cgs))

getStackSpace :: CGState -> Int
getStackSpace cgs = (sp cgs + 1) * 8

newVar :: Monad m => String -> StateT CGState m ()
newVar name = state (\cgs ->
  let newSP = sp cgs + 1
      newVarMap = Map.insert name newSP (vars cgs)
  in ((), cgs { vars = newVarMap, sp = newSP }))

handleLet :: A.Node -> StateT CGState IO ()
handleLet (A.Leaf (A.NameVal _ name)) = do
  cgs <- get
  let maybeOffset = getOffset cgs name
  if isJust maybeOffset
    then w2f $ Asm.exprToStack (fromJust maybeOffset)
    else newVar name >> (w2f $ Asm.newVar ++ (Asm.exprToStack 0))
    -- 0 because we always know where a new variable is going -- maybe this
    -- will change at some point?

handleNameExpr :: String -> StateT CGState IO ()
handleNameExpr name = do
  cgs <- get
  let maybeOffset = getOffset cgs name
  if isJust maybeOffset
    --then return $ Asm.movVarRax (fromJust maybeOffset)
    then w2f $ Asm.movVarRax (fromJust maybeOffset)
    else undefined


addProc :: String -> [Proc] -> CGState -> ((), CGState)
addProc name proc cgs =
  let newMap = Map.insert name proc (procs cgs)
  in ((), cgs { procs = newMap })

-- This only mutates the CGState and does not write any assembly because the
-- procedure definitions will all happen at the end. 
handleProcDef :: A.Node -> A.Node -> StateT CGState IO ()
handleProcDef (A.Leaf (A.NameVal A.ProcType name)) stl = do
  cgs <- get
  let maybeProc = Map.lookup name (procs cgs) 
  let newProc = Proc stl Nothing
  if isJust maybeProc
    then state (addProc name (newProc:(fromJust maybeProc)))
    else state (addProc name [newProc])

-- Should this have lazy semantics?
handleProcCall :: A.Node -> StateT CGState IO ()
handleProcCall (A.Leaf (A.NameVal A.ProcType name))  = w2f $ Asm.callName name

--makeVal :: A.Node -> State CGState T.Text
makeVal :: A.Node -> CGState -> T.Text
makeVal (A.Leaf (A.NameVal _ name)) cgs =
  Asm.makeNameVal $ fromJust $ getOffset cgs name
makeVal (A.Leaf (A.IntVal val)) _ = Asm.makeIntVal val

build :: A.Node -> StateT CGState IO ()

build (A.Node A.Program cs) = do
  sequence_ $ intersperse (w2f ["\n"]) (map build cs)
  cgs <- get
  w2f (Asm.adjustRsp (getStackSpace cgs))

build (A.Node A.ProcDef (name:def:[])) = handleProcDef name def
build (A.Node A.ProcCall (name:[])) = handleProcCall name

build (A.Node A.ShowExpr (c:[])) = (build c) >> (w2f Asm.showAsm)
-- Refactor this into something nicer when I feel like it
build (A.Node A.AddExpr (val:expr:[])) =
  (build expr) >> get >>= (w2f . Asm.addVal . (makeVal val))
--build (A.Node A.SubExpr (val:expr:[])) =
--  (build expr) >> (w2f $ Asm.subVal val)
--build (A.Node A.MulExpr (val:expr:[])) =
--  (build expr) >> (w2f $ Asm.mulVal $ val)
build (A.Leaf (A.IntExpr val)) =  w2f (Asm.movValRax val)
build (A.Node A.LetExpr (n:e:[])) = (build e) >> (handleLet n)
build (A.Leaf (A.NameExpr _ name)) = handleNameExpr name

build x = (trace (show x) undefined)

w2f :: [T.Text] -> StateT CGState IO ()
w2f ts = get >>= (\cgs -> liftIO $ (cgwrite cgs) ts)

generateAsm :: A.Node -> IO ()
generateAsm ast = do
  let fn = "asm/main.s"
  handle <- openFile fn WriteMode
  TIO.hPutStr handle Asm.preamble

  --evalState (build ast) (initCGState handle)
  runStateT (build ast) (initCGState handle)
  --mapM_ (TIO.hPutStr handle) (Asm.preamble : program)

  TIO.hPutStr handle Asm.postlude
  hClose handle
