{-# Language OverloadedStrings #-}
module CodeGen
  ( generateAsm
  ) where

--import Data.HashMap.Strict
import Data.List (intersperse, nubBy)
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

data Var = IntVar Asm.Store | ProcVar Proc
  deriving Show

varToStore :: Var -> Asm.Store
varToStore (IntVar s) = s
varToStore (ProcVar ProcRef{refstore=s}) = s
varToStore (ProcVar Proc{procname=n}) =
  error $ "Nonref method " ++ n ++ " has no store!"

type VarMap = Map.Map String Var

                  -- paramname?
data Param = Param { paramvar :: String
                   --, store :: Asm.Store
                   , procorcond :: Either Proc (Maybe A.Node) }
                   deriving Show


--data Arg = VarArg Asm.Store | ProcArg Proc
  --deriving Show

data Proc = Proc { procname :: String
                 , procdefs :: [([Param], A.Node)]
                 , argmap :: VarMap }
          | ProcRef { refstore :: Asm.Store
                    , argmap :: VarMap }

-- START should pstroes be more like Map.Map String SkeletalParam
-- data SkeletalParam = VarParam Asm.Store | ProcParam Asm.Store Proc
--                                                               (ProcRef)
-- Maybe I need to reintroduce cond


isNonRefProc :: Var -> Bool
isNonRefProc (ProcVar Proc{}) = True
isNonRefProc _ = False

isStoreVar = not . isNonRefProc

filterNonRefProcs :: VarMap -> [Proc]
filterNonRefProcs =
  let varToProc x = case x of (ProcVar proc) -> proc
  in  (map varToProc) . (filter isNonRefProc) . Map.elems

instance Show Proc where
  show Proc{procname=n} = "Proc " ++ n
  show ProcRef{refstore=r}  = "ProcRef " ++ (show r)


data CGState = CGState { vars :: VarMap
                       --, procs :: Map.Map String Proc
                       , procprefix :: String
                       , sp :: Int
                       , cgwrite :: [T.Text] -> IO () }

getVar :: CGState -> String -> Maybe Asm.Store
getVar cgs name =
  let var = Map.lookup name (vars cgs)
  in case var of
       (Just (IntVar (Asm.Stack x))) -> Just $ Asm.Stack $ x - (sp cgs)
       Nothing -> Nothing

getStackSpace :: CGState -> Int
getStackSpace cgs = 8 - (sp cgs)

newStackVar :: Monad m => String -> StateT CGState m Asm.Store
newStackVar name = state (\cgs ->
  let newSP = sp cgs - 8
      newStore = Asm.Stack newSP
      newVarMap = Map.insert name (IntVar newStore) (vars cgs)
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
    Nothing  -> error $ "Could not getVar " ++ name

makeParam :: A.Node -> (Param, Asm.Store -> Var)
makeParam (A.Node A.Cond ((A.Leaf (A.NameVal A.IntType name)):_))
  -- = Param { paramvar = name, store = store, procorcond = Right Nothing }
  = (Param { paramvar = name, procorcond = Right Nothing }, IntVar)
-- TODO actualy take conds into account
-- And procs...
makeParam x = error (show x)

--handleAdditionalProcDef :
handleNewProcDef :: String ->  A.Node -> [A.Node] -> Proc
handleNewProcDef name procDef paramNodes =
  let paramsAndArgs = map makeParam $ paramNodes
      uniquePaa = nubBy (\x y -> paramvar (fst x) == (paramvar $ fst y)) paramsAndArgs
      names = map (paramvar . fst) uniquePaa
      vars = zipWith id (map snd uniquePaa) Asm.argRegs
      argMap = Map.fromList $ zip names vars
      params = map fst paramsAndArgs
  in  Proc name [(params, procDef)] argMap

addProc :: String -> Proc -> CGState -> ((), CGState)
addProc name proc cgs =
  let newMap = Map.insert name (ProcVar proc) (vars cgs)
  in  ((), cgs { vars = newMap })

-- This only mutates the CGState and does not write any assembly because the
-- procedure definitions will all happen at the end. 
handleProcDef :: [A.Node] -> A.Node -> A.Node -> StateT CGState IO ()
handleProcDef condNodes (A.Leaf (A.NameVal A.ProcType name)) def = do
  cgs <- get
  -- How do we know this is a Proc?
  let maybeProc = Map.lookup name (vars cgs) 
  let newProc = case maybeProc of
                  (Just proc) -> error "I can't do this yet."
                  otherwise -> handleNewProcDef name def condNodes
  state (addProc name newProc)

-- Can't I just use this (instead of the two above)
--addParam :: Proc -> String -> Param -> CGState -> ((), CGState)
--addParam proc procName arg cgs =
--  let newArgMap = Map.insert procName (store arg) (argmap proc)
--      newProc = (proc { argmap = newArgMap })
--      newMap = Map.insert (procname proc) newProc (vars cgs)
--  in ((), cgs { vars = newMap })

nameIsProcType :: A.Node -> Bool
nameIsProcType (A.Leaf (A.NameVal A.ProcType _)) = True
nameIsProcType _ = False

getProcName (A.Leaf (A.NameVal A.ProcType name)) = name
getProcName _ = undefined
 
--filterVarArgs :: [Param] -> VarMap
--filterVarArgs =
--  let isVar x = case procorcond x of (Right _) -> True; _ -> False
--      toPair x = (paramvar x, store x)
--  in  Map.fromList . (map toPair) . (filter isVar)
--
--filterProcArgs :: Map.Map a Param -> Map.Map a Proc
--filterProcArgs = undefined
---- We just need the arg mapping
--  --let isProc x = case x of (ProcArg _) -> True; _ -> False
--  --    toProc x = case x of (ProcArg p) -> p
--  --in  (Map.map toProc) . (Map.filter isProc)


-- TODO I forgot what this does
pushArgRegisters :: StateT CGState IO ()
pushArgRegisters =
  let
    toStack key store = case store of
      reg@(Asm.Register _) -> do
        w2f $ Asm.newStackVar
        newStore <- newStackVar key
        w2f $ Asm.storeToStore reg newStore
      _ -> return ()
  -- Could this just be mapM?
    stores = (Map.map varToStore) . (Map.filter isStoreVar) . vars
  in get >>= (sequence_ . (Map.mapWithKey toStack) . stores)

unpackArg :: VarMap -> A.Node -> (Var, A.Node)
unpackArg varMap (A.Node A.ArgAssign ((A.Leaf (A.NameVal dtype name)):expr:[])) =
  let var = case (Map.lookup name varMap) of
              (Just var) -> var
              Nothing -> error $ "Could not find argument: " ++ name
  in  (var, expr)

handleProcCall :: [A.Node] -> A.Node -> StateT CGState IO ()
handleProcCall rawArgs (A.Leaf (A.NameVal A.ProcType procName)) = do
  cgs <- get
  let proc = case (vars cgs) Map.! procName of
                (ProcVar proc) -> proc
  let args = map (unpackArg (argmap proc)) rawArgs
  -- This will fail if it is not a procref
  let assign x = case x of
                  (ProcVar ProcRef{refstore=rs}, expr) ->
                    w2f $ Asm.procToStore (getProcName expr) rs
                  (IntVar store, expr) ->
                    build expr >> (w2f $ Asm.accToStore store)
                 -- This is questionable 
  mapM_ assign args
  pushArgRegisters
  case proc of 
    Proc{procname=n} -> w2f $ Asm.callName $ (procprefix cgs) ++ n
    ProcRef{refstore=store} -> w2f $ Asm.callStore store

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
  let condList = case conds of (A.Node A.CondList cs) -> cs
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
  let nonRefProcs = filterNonRefProcs (vars final)
  forM_ nonRefProcs $ \proc -> do
    let name = procname proc
    (cgwrite init) $ Asm.beginProc $ (procprefix final) ++ name
    let newInit = init { procprefix = (procprefix init) ++ name ++ "_"
                       , vars = argmap proc
                       }
                       --, procs = filterProcArgs $ fst $ procdefs proc }
                       --, procs = undefined }
    -- TODO make this accomodate conds
    buildHelper newInit (snd $ head $ procdefs  proc)
      

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
                         --, procs = Map.empty
                         , procprefix = ""
                         , sp = 8
                         , cgwrite = (mapM_ (TIO.hPutStr h)) }

