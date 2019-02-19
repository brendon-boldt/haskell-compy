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

data CGState = CGState { vars :: Map.Map String Int
                       , procs :: Map.Map String [Proc]
                       , sp :: Int
                       , cgwrite :: [T.Text] -> IO () }

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
handleProcDef (A.Leaf (A.NameVal A.ProcType name)) def = do
  cgs <- get
  let maybeProc = Map.lookup name (procs cgs) 
  let newProc = Proc def Nothing
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

handleRoot :: [A.Node] -> StateT CGState IO ()
handleRoot stl = do
  sequence_ $ intersperse (w2f ["\n"]) (map build stl)
  cgs <- get
  --w2f (Asm.adjustRsp (getStackSpace cgs))
  w2f (Asm.endProc (getStackSpace cgs))

build :: A.Node -> StateT CGState IO ()

build (A.Node A.Program stl) = handleRoot stl
build (A.Node A.Def stl) = handleRoot stl

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


buildProcs :: CGState -> CGState -> IO ()
buildProcs final init = do
  -- Temporary simplifying assumption: no conds, single def
  forM_ (Map.keys $ procs final) $ \name -> do
    forM_ ((procs final) Map.! name) $ \proc -> do

      (cgwrite init) $ Asm.beginProc name
      runStateT (build (procdef proc)) init

buildHelper :: CGState -> A.Node -> IO ()
buildHelper initState root = do
  finalState <- runStateT (build root) initState
  buildProcs (snd finalState) initState

generateAsm :: A.Node -> IO ()
generateAsm ast = do
  let fn = "asm/main.s"
  handle <- openFile fn WriteMode
  let initState = getCGState handle

  TIO.hPutStr handle Asm.preamble

  buildHelper initState ast

  hClose handle
    where getCGState h = CGState
                         Map.empty
                         Map.empty
                         (-1)
                         (mapM_ (TIO.hPutStr h))
