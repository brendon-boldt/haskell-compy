{-# Language OverloadedStrings #-}
module CodeGen
  ( generateAsm
  ) where

--import Data.HashMap.Strict
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad
--import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.State
import System.IO
import Data.Foldable

import Debug.Trace

import qualified AST as A
import qualified Asm as Asm

data Cond = Cond { condexprs :: (A.Node, A.Node)
                 , condorder :: Ordering }

data Arg = VarArg Asm.Store | ProcArg Proc

data Proc = Proc { procname :: String
                 , procdefs :: [([Cond], A.Node)]
                 --, args :: Map.Map String Asm.Store }
                 , args :: Map.Map String Arg }

data CGState = CGState { vars :: Map.Map String Asm.Store
                       , procs :: Map.Map String Proc
                       , procprefix :: String
                       , sp :: Int
                       , cgwrite :: [T.Text] -> IO () }

getVar :: CGState -> String -> Maybe Asm.Store
getVar cgs name =
  let var = Map.lookup name (vars cgs)
  in case var of
       (Just (Asm.Stack x)) -> Just $ Asm.Stack $ x - (sp cgs)
       x -> x

getStackSpace :: CGState -> Int
getStackSpace cgs = 8 - (sp cgs)

newStackVar :: Monad m => String -> StateT CGState m Asm.Store
newStackVar name = state (\cgs ->
  let newSP = sp cgs - 8
      newStore = Asm.Stack newSP
      newVarMap = Map.insert name newStore (vars cgs)
  -- The new stack var will always be 0 offset from %rsp
  in (Asm.Stack 0, cgs { vars = newVarMap, sp = newSP }))

handleLet :: A.Node -> StateT CGState IO ()
handleLet (A.Leaf (A.NameVal _ name)) = do
  cgs <- get
  let maybeVar = getVar cgs name
  case maybeVar of
    (Just var) -> w2f $ Asm.accToStore var
    otherwise  -> do
      w2f $ Asm.newStackVar
      newStackVar name >>= (w2f . Asm.accToStore)

handleNameExpr :: String -> StateT CGState IO ()
handleNameExpr name = do
  cgs <- get
  let maybeVar = getVar cgs name
  case maybeVar of
    (Just var) -> w2f $ Asm.movToAcc var
    otherwise  -> (trace name undefined)

makeCond :: A.Node -> Cond
makeCond _ = undefined

addProc :: String -> Proc -> CGState -> ((), CGState)
addProc name proc cgs =
  let newMap = Map.insert name proc (procs cgs)
  in  ((), cgs { procs = newMap })

-- This only mutates the CGState and does not write any assembly because the
-- procedure definitions will all happen at the end. 
handleProcDef :: [A.Node] -> A.Node -> A.Node -> StateT CGState IO ()
handleProcDef conds (A.Leaf (A.NameVal A.ProcType name)) def = do
  cgs <- get
  let maybeProc = Map.lookup name (procs cgs) 
  let newProcdef = (map makeCond conds, def)
  let consProcdef x y = y { procdefs = x : (procdefs y) }
  let newProc = (case maybeProc of
                  (Just proc) -> consProcdef newProcdef proc
                  _ -> Proc name [newProcdef] Map.empty)
  state (addProc name newProc)

addVarArg :: Proc -> String -> Asm.Store -> CGState -> ((), CGState)
addVarArg proc varName reg cgs =
  -- It's okay if this fails
  let newArgMap = Map.insert varName (VarArg reg) (args proc)
      newProc = (proc { args = newArgMap })
      newMap = Map.insert (procname proc) newProc (procs cgs)
  in ((), cgs { procs = newMap })

addProcArg :: Proc -> String -> Proc -> CGState -> ((), CGState)
addProcArg proc procName procArg cgs =
  -- It's okay if this fails
  let newArgMap = Map.insert procName (ProcArg procArg) (args proc)
      newProc = (proc { args = newArgMap })
      newMap = Map.insert (procname proc) newProc (procs cgs)
  in ((), cgs { procs = newMap })

-- TODO figure a better way to match args and regs
handleArgAssign :: String -> (String, A.Node) -> StateT CGState IO ()
handleArgAssign procName (varName, expr) = do
  cgs <- get
  let proc = (procs cgs) Map.! procName
  let reg = if Map.member varName (args proc)
              then (args proc) Map.! varName
              else Asm.argRegs !! (length $ args proc)
  state (addVarArg proc varName reg)
  build expr
  w2f $ Asm.accToStore reg
  --return $ A.value varname

filterVarArgs :: Map.Map a Arg -> Map.Map a Asm.Store
filterVarArgs =
  let isVar x = case x of (VarArg _) -> True; _ -> False
      toVar x = case x of (VarArg v) -> v
  in  (Map.map toVar) . (Map.filter isVar)

filterProcArgs :: Map.Map a Arg -> Map.Map a Proc
filterProcArgs =
  let isProc x = case x of (ProcArg _) -> True; _ -> False
      toProc x = case x of (ProcArg p) -> p
  in  (Map.map toProc) . (Map.filter isProc)


pushArgRegisters :: StateT CGState IO ()
pushArgRegisters =
  let
    toStack key store = case store of
      r@(Asm.Register _) -> do
        w2f $ Asm.newStackVar
        var <- newStackVar key
        w2f $ Asm.storeToStore r var
      _ -> return ()
  -- Could this just be mapM?
  in get >>= (sequence_ . (Map.mapWithKey toStack) . vars)

unpackArg :: A.Node -> (String, A.Node)
unpackArg (A.Node A.ArgAssign ((A.Leaf (A.NameVal _ name)):expr:[])) =
  (name, expr)

handleProcCall :: [A.Node] -> A.Node -> StateT CGState IO ()
handleProcCall procArgs (A.Leaf (A.NameVal A.ProcType name)) = do
  mapM_ ((handleArgAssign name) . unpackArg) procArgs
  pushArgRegisters
  cgs <- get
  w2f $ Asm.callName $ (procprefix cgs) ++ name

makeVal :: A.Node -> CGState -> Asm.Store
makeVal (A.Leaf (A.NameVal _ name)) cgs =
  case getVar cgs name of (Just x) -> x
makeVal (A.Leaf (A.IntVal val)) _ = Asm.Literal val

handleRoot :: [A.Node] -> StateT CGState IO ()
handleRoot stl = do
  sequence_ $ intersperse (w2f ["\n"]) (map build stl)
  cgs <- get
  --w2f (Asm.adjustRsp (getStackSpace cgs))
  w2f (Asm.endProc (getStackSpace cgs))

build :: A.Node -> StateT CGState IO ()

build (A.Node A.Program stl) = handleRoot stl
build (A.Node A.Def stl) = handleRoot stl

build (A.Node A.ProcDef (name:def:[])) = handleProcDef [] name def
build (A.Node A.ProcDef (conds:name:def:[])) =
  let condList = case conds of (A.Node _ cs) -> cs
  in handleProcDef condList name def
build (A.Node A.ProcCall (name:[])) = handleProcCall [] name
build (A.Node A.ProcCall (args:name:[])) =
  handleProcCall (A.children args) name

build (A.Node A.ShowExpr (c:[])) = do
  pushArgRegisters
  build c
  w2f Asm.showAcc
-- Refactor this into something nicer when I feel like it
build (A.Node A.AddExpr (val:expr:[])) =
  (build expr) >> get >>= (w2f . Asm.addToAcc . (makeVal val))
--build (A.Node A.SubExpr (val:expr:[])) =
--  (build expr) >> (w2f $ Asm.subVal val)
--build (A.Node A.MulExpr (val:expr:[])) =
--  (build expr) >> (w2f $ Asm.mulVal $ val)
build (A.Leaf (A.IntExpr val)) =  w2f (Asm.movToAcc $ Asm.Literal val)
build (A.Node A.LetExpr (n:e:[])) = (build e) >> (handleLet n)
build (A.Leaf (A.NameExpr _ name)) = handleNameExpr name

build x = (trace (show x) undefined)

w2f :: [T.Text] -> StateT CGState IO ()
w2f ts = get >>= (\cgs -> liftIO $ (cgwrite cgs) ts)


buildProcs :: CGState -> CGState -> IO ()
buildProcs final init = do
  -- Temporary simplifying assumption: no conds, single def
  --forM_ (Map.keys $ procs final) $ \name -> do
    --forM_ ((procs final) Map.! name) $ \proc -> do
  forM_ (procs final) $ \proc -> do
    let name = procname proc
    (cgwrite init) $ Asm.beginProc $ (procprefix final) ++ name
    let newInit = init { procprefix = (procprefix init) ++ name ++ "_"
                       , vars = filterVarArgs $ args proc }
    -- TODO make this accomodate conds
    buildHelper newInit (snd $ head $ procdefs proc)
      

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
                         { vars = Map.empty
                         , procs = Map.empty
                         , procprefix = ""
                         , sp = 8
                         , cgwrite = (mapM_ (TIO.hPutStr h)) }

