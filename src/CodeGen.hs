{-# Language OverloadedStrings, RankNTypes #-}
module CodeGen
  ( generateAsm
  ) where

--import Data.HashMap.Strict
import Data.List (intersperse, sortOn, groupBy)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.State
import System.IO
import Data.Foldable
import Data.Maybe (isJust, fromJust)

import Debug.Trace

import qualified AST as A
import qualified Asm as Asm

data Store = RawStore Asm.Store
  deriving Show

data Var = IntVar Store | ProcVar Proc
  deriving Show

type VarMap = Map.Map String Var

                  -- paramname?
data Param = Param { paramvar :: String
                   , cond :: Maybe (Ordering, A.Node) }
                   deriving Show


data Proc = Proc { procname :: String
                 , procdefs :: [([Param], A.Node)]
                 , argmap :: Either VarMap String }
          | ProcRef { refstore :: Store
                    , argmap :: Either VarMap String }

justVarMap :: Either VarMap String -> VarMap
justVarMap (Left vm) = vm
justVarMap (Right s) = error $ s ++ " isn't a VarMap!"


varToStore :: Var -> Store
varToStore (IntVar s) = s
varToStore (ProcVar ProcRef{refstore=s}) = s
varToStore (ProcVar Proc{procname=n}) =
  error $ "Nonref method " ++ n ++ " has no store!"

varToProc :: Var -> Proc
varToProc (ProcVar proc) = proc
varToProc _ = error "That wasn't a Proc!"

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
                       , procprefix :: String
                       , sp :: Int
                       , cgwrite :: [T.Text] -> IO () }


type CGST a = forall m. Monad m => StateT CGState m a

-- TODO use push and pop instead of manually adjusting stack

resolveStore :: Store -> CGST Asm.Store
resolveStore (RawStore (Asm.Stack x)) = do
  cgs <- get
  return $ Asm.Stack $ x - (sp cgs)
resolveStore (RawStore s) = return s

getVar :: String -> CGST (Maybe Asm.Store)
getVar name = do
  cgs <- get
  let var = Map.lookup name $ vars cgs
  case var of
    (Just (IntVar s)) -> liftM Just $ resolveStore s
    Nothing  -> return Nothing

getStackSpace :: CGState -> Int
getStackSpace cgs = 8 - (sp cgs)

-- TODO rename this to something less misleading
newStackVar :: String -> CGST Store
newStackVar name = state (\cgs ->
  let newSP = sp cgs - 8
      newStore = RawStore $ Asm.Stack newSP
      newVarMap = Map.insert name (IntVar newStore) (vars cgs)
  in (newStore, cgs { vars = newVarMap, sp = newSP }))


regVarToStack :: String -> Var -> CGST Store
regVarToStack name var = state (\cgs ->
  let newSP = sp cgs - 8
      newStore = RawStore $ Asm.Stack newSP
      newVar = case var of
        (IntVar _)   -> IntVar newStore
        (ProcVar pr@ProcRef{}) -> ProcVar $ pr { refstore = newStore }
      newVarMap = Map.insert name newVar (vars cgs)
  in (newStore, cgs { vars = newVarMap, sp = newSP }))


handleLet :: A.Node -> StateT CGState IO ()
handleLet (A.Leaf (A.NameVal _ name)) = do
  maybeVar <- getVar name
  case maybeVar of
    (Just var) -> w2f $ Asm.accToStore var
    Nothing    -> newStackVar name >> (w2f $ Asm.pushAcc)

handleNameExpr :: String -> StateT CGState IO ()
handleNameExpr name = do
  maybeVar <- getVar name
  case maybeVar of
    (Just var) -> w2f $ Asm.movToAcc var
    Nothing  -> error $ "Could not getVar " ++ name


makeParam :: A.Node -> (Param, Store -> Var)
makeParam (A.Node A.Cond ((A.Leaf (A.NameVal A.IntType name)):[]))
  = (Param { paramvar = name, cond = Nothing }, IntVar)

makeParam (A.Node A.Cond ( (A.Leaf (A.NameVal A.IntType name))
                         : (A.Leaf (A.OrderVal order))
                         : condExpr
                         : []))
  = (Param { paramvar = name, cond = Just (order, condExpr) }, IntVar)

makeParam (A.Node A.Cond ((A.Leaf (A.NameVal A.ProcType name)):[]))
  = error $ "Proc param " ++ name ++  " must be \"like\" a prototype"

makeParam (A.Node A.Cond ( (A.Leaf (A.NameVal A.ProcType name))
                         : (A.Leaf (A.NameVal A.ProcType protoname))
                         : []))
  = (Param { paramvar = name, cond = Nothing }, \s -> ProcVar (ProcRef { refstore = s, argmap = Right protoname }))

makeParam x = error $ "Could not makeParam with: " ++ (show x)


handleNewProcDef :: String ->  A.Node -> [A.Node] -> Proc
handleNewProcDef name procDef paramNodes =
  let paramsAndArgs = map makeParam $ paramNodes
      uniquePaa = (sortOn $ paramvar . fst)
                $ (map head)
                $ (groupBy (\x y ->
                  (paramvar $ fst x) == (paramvar $ fst y)))
                paramsAndArgs
      names = map (paramvar . fst) uniquePaa
      vars = zipWith id (map snd uniquePaa) (map RawStore Asm.argRegs)
      argMap = Map.fromList $ zip names vars
      params = map fst paramsAndArgs
  in  Proc name [(params, procDef)] (Left argMap)

insertProc :: String -> Proc -> CGState -> ((), CGState)
insertProc name proc cgs =
  let newMap = Map.insert name (ProcVar proc) (vars cgs)
  in  ((), cgs { vars = newMap })

resolveArgMaps :: String -> Proc -> CGState -> ((), CGState)
resolveArgMaps name proc cgs =
  let replaceArgMap arg = case arg of
        (ProcVar pr@ProcRef{argmap=(Right proto)}) -> ProcVar $ pr { argmap = argmap $ varToProc $ (vars cgs) Map.! proto }
        x -> x
      newArgMap = Map.map replaceArgMap (justVarMap $ argmap proc)
      newProc = proc { argmap = Left newArgMap }
      newMap = Map.insert name (ProcVar newProc) (vars cgs)
  in  ((), cgs { vars = newMap })


-- TODO check that the parameters are the same
appendProcDef :: Proc -> A.Node -> [A.Node] -> Proc
appendProcDef proc def paramNodes =
  let params = map (fst . makeParam) paramNodes
  in  proc { procdefs = (params, def) : (procdefs proc) }

handleProcDef :: [A.Node] -> A.Node -> A.Node -> CGST ()
handleProcDef condNodes (A.Leaf (A.NameVal A.ProcType name)) def = do
  cgs <- get
  -- How do we know this is a Proc?
  let maybeProc = Map.lookup name (vars cgs)
  let newProc = case maybeProc of
                  -- Not handling ProcRefs is a simplfiying assumption
                  (Just (ProcVar proc@Proc{})) -> appendProcDef proc def condNodes
                  Nothing -> handleNewProcDef name def condNodes
  state (insertProc name newProc)
  case maybeProc of
    Nothing   -> state (resolveArgMaps name newProc)
    otherwise -> return ()

nameIsProcType :: A.Node -> Bool
nameIsProcType (A.Leaf (A.NameVal A.ProcType _)) = True
nameIsProcType _ = False

getProcName (A.Leaf (A.NameVal A.ProcType name)) = name
getProcName _ = undefined

-- Push argument registers to the stack in preparation for a proc call
-- pushOldArgRegisters ?
pushArgRegisters :: StateT CGState IO ()
pushArgRegisters =
  let
    toStack key var = case varToStore var of
      (RawStore reg@(Asm.Register _)) -> do
        regVarToStack key var
        w2f $ Asm.push reg
      _ -> return ()
  -- Could this just be mapM?
    stores = (Map.filter isStoreVar) . vars
  in get >>= (sequence_ . (Map.mapWithKey toStack) . stores)

unpackArg :: VarMap -> A.Node -> (Var, A.Node)
unpackArg varMap (A.Node A.ArgAssign ((A.Leaf (A.NameVal dtype name)):expr:[])) =
  let var = case (Map.lookup name varMap) of
              (Just var) -> var
              Nothing -> error $ "Could not find argument: " ++ name
  in  (var, expr)

procToStore :: String -> Store -> StateT CGState IO ()
procToStore argName targetStore = do
  cgs <- get
  case (vars cgs) Map.! argName of
    (ProcVar Proc{procname=n}) -> do
      asmTargetStore <- resolveStore targetStore
      w2f $ Asm.procToStore n asmTargetStore
    (ProcVar ProcRef{refstore=rs}) -> do
      asmRefStore <- resolveStore rs
      asmTargetStore <- resolveStore targetStore
      w2f $ Asm.storeToStore asmRefStore asmTargetStore
    x -> error $ (show x) ++ " cannot be moved to stored as if it were a proc."

handleProcCall :: [A.Node] -> A.Node -> StateT CGState IO ()
handleProcCall rawArgs (A.Leaf (A.NameVal A.ProcType procName)) = do
  cgs <- get
  let proc = case (vars cgs) Map.! procName of
                (ProcVar proc) -> proc
                x -> error $ "here: " ++ (show $ vars cgs)
  let args = map (unpackArg (justVarMap $ argmap proc)) rawArgs
  -- This will fail if it is not a procref
  let assign x = case x of
                  (ProcVar ProcRef{refstore=rs}, expr) ->
                    procToStore (getProcName expr) rs
                  (IntVar store, expr) -> do
                    build expr
                    asmStore <- resolveStore store
                    w2f $ Asm.accToStore asmStore
                 -- This is questionable
  pushArgRegisters
  mapM_ assign args
  updatedCgs <- get
  let updatedProc = case vars updatedCgs Map.! procName of
                      (ProcVar proc) -> proc
  case updatedProc of
    Proc{procname=n} -> w2f $ Asm.callName $ (procprefix cgs) ++ n
    ProcRef{refstore=store} -> do
      asmStore <- resolveStore store
      w2f $ Asm.callStore asmStore

makeVal :: A.Node -> CGST Asm.Store
makeVal (A.Leaf (A.NameVal _ name)) = do
  var <- getVar name
  return $ case var of (Just x) -> x
makeVal (A.Leaf (A.IntVal val)) = return $ Asm.Literal val

handleRoot :: [A.Node] -> StateT CGState IO ()
handleRoot stl = do
  --sequence_ $ intersperse (w2f ["\n"]) (map build stl)
  sequence_ $ (map build stl)
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
build (A.Node A.AddExpr (val:expr:[])) = do
  build expr
  asmVal <- makeVal val
  w2f $ Asm.addToAcc asmVal
build (A.Node A.SubExpr (val:expr:[])) = do
  build expr
  asmVal <- makeVal val
  w2f $ Asm.subFromAcc asmVal
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


buildConds :: String -> [[Param]] -> StateT CGState IO ()
buildConds procName paramsList = do
  forM_ (zip [0..] paramsList) $ \(num, params) -> do
    let condLabel = (procName ++ "c" ++ (show num))
    let procLabel = (procName ++ "d" ++ (show num))
    forM_ (filter (isJust . cond) params) $ \param -> do
      let (ord, node) = fromJust $ cond param
      build node
      cgs <- get
      let store = varToStore $ vars cgs Map.! (paramvar param)
      asmStore <- resolveStore store
      w2f $ Asm.condJump condLabel asmStore ord
    -- TODO Rename beginProc
    w2f $ Asm.jump procLabel
    w2f $ Asm.beginProc condLabel
  w2f $ Asm.exitWith 2

buildProcs :: CGState -> CGState -> IO ()
buildProcs final init = do
  let nonRefProcs = filterNonRefProcs (vars final)
  forM_ nonRefProcs $ \proc -> do
    let name = procname proc
    let newInit = init { procprefix = (procprefix init) ++ name ++ "_"
                       , vars = justVarMap $ argmap proc
                       }
    let procName = (procprefix final) ++ name
    (cgwrite init) $ Asm.beginProc procName
    let paramsList = reverse $ map fst $ procdefs proc
    let procList = reverse $ procdefs proc
    newNewInit <- execStateT (buildConds procName paramsList) newInit
    forM_ (zip [0..] procList) $ \(num, def) -> do
      (cgwrite init) $ Asm.beginProc $ procName ++ "d" ++ (show num)
      buildHelper newNewInit $ snd def

buildHelper :: CGState -> A.Node -> IO ()
buildHelper initState root = do
  finalState <- execStateT (build root) initState
  buildProcs finalState initState

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
                         , procprefix = ""
                         , sp = 8
                         , cgwrite = (mapM_ (TIO.hPutStr h)) }

