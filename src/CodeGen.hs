{-# Language OverloadedStrings #-}
module CodeGen
  ( generateAsm
  ) where

--import Data.HashMap.Strict
import Data.List (intersperse, sortOn, groupBy)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad
--import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.State
import System.IO
import Data.Foldable
import Data.Maybe (isJust, fromJust)

import Debug.Trace

import qualified AST as A
import qualified Asm as Asm

data Var = IntVar Asm.Store | ProcVar Proc
  deriving Show

type VarMap = Map.Map String Var

                  -- paramname?
data Param = Param { paramvar :: String
                   --, store :: Asm.Store
                   , cond :: Maybe (Ordering, A.Node) }
                   deriving Show


--data Arg = VarArg Asm.Store | ProcArg Proc
  --deriving Show

data Proc = Proc { procname :: String
                 , procdefs :: [([Param], A.Node)]
                 --, argmap :: VarMap }
                 , argmap :: Either VarMap String }
          | ProcRef { refstore :: Asm.Store
                    --, argmap :: VarMap }
                    , argmap :: Either VarMap String }

justVarMap :: Either VarMap String -> VarMap
justVarMap (Left vm) = vm
justVarMap (Right s) = error $ s ++ " isn't a VarMap!"

-- START should pstroes be more like Map.Map String SkeletalParam
-- data SkeletalParam = VarParam Asm.Store | ProcParam Asm.Store Proc
--                                                               (ProcRef)
-- Maybe I need to reintroduce cond

varToStore :: Var -> Asm.Store
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
                       --, procs :: Map.Map String Proc
                       , procprefix :: String
                       , sp :: Int
                       , cgwrite :: [T.Text] -> IO () }


resolveStore :: CGState -> Asm.Store -> Asm.Store
resolveStore cgs (Asm.Stack x) = Asm.Stack $ x - (sp cgs)
resolveStore cgs s = s

getVar :: CGState -> String -> Maybe Asm.Store
getVar cgs name =
  let var = Map.lookup name (vars cgs)
  in  case var of
    (Just (IntVar s)) -> Just $ resolveStore cgs s
    Nothing  -> Nothing

getStackSpace :: CGState -> Int
getStackSpace cgs = 8 - (sp cgs)

-- TODO rename this to something less misleading
newStackVar :: Monad m => String -> StateT CGState m Asm.Store
newStackVar name = state (\cgs ->
  let newSP = sp cgs - 8
      newStore = Asm.Stack newSP
      newVarMap = Map.insert name (IntVar newStore) (vars cgs)
  -- The new stack var will always be 0 offset from %rsp
  in (Asm.Stack 0, cgs { vars = newVarMap, sp = newSP }))


regVarToStack :: Monad m => String -> Var -> StateT CGState m Asm.Store
regVarToStack name var = state (\cgs ->
  let newSP = sp cgs - 8
      newStore = Asm.Stack newSP
      newVar = case var of
        (IntVar _)   -> IntVar newStore
        (ProcVar pr@ProcRef{}) -> ProcVar $ pr { refstore = newStore }
      newVarMap = Map.insert name newVar (vars cgs)
  -- The new stack var will always be 0 offset from %rsp
  in (Asm.Stack 0, cgs { vars = newVarMap, sp = newSP }))


handleLet :: A.Node -> StateT CGState IO ()
handleLet (A.Leaf (A.NameVal _ name)) = do
  cgs <- get
  let maybeVar = getVar cgs name
  case maybeVar of
    (Just var) -> w2f $ Asm.accToStore var
    Nothing    -> do
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
makeParam (A.Node A.Cond ((A.Leaf (A.NameVal A.IntType name)):[]))
  = (Param { paramvar = name, cond = Nothing }, IntVar)

makeParam (A.Node A.Cond ( (A.Leaf (A.NameVal A.IntType name))
                         : (A.Leaf (A.OrderVal order))
                         -- : (A.Node A.Expr condExpr)
                         : condExpr
                         : []))
  = (Param { paramvar = name, cond = Just (order, condExpr) }, IntVar)

makeParam (A.Node A.Cond ((A.Leaf (A.NameVal A.ProcType name)):[]))
  = error $ "Proc param " ++ name ++  " must be \"like\" a prototype"

makeParam (A.Node A.Cond ( (A.Leaf (A.NameVal A.ProcType name))
                         : (A.Leaf (A.NameVal A.ProcType protoname))
                         : []))
  = (Param { paramvar = name, cond = (error "When do we need this proc?") }, \s -> ProcVar (ProcRef { refstore = s, argmap = Right protoname }))

--makeParam x = error (show x)
makeParam x = error "Could not makeParam."


--handleAdditionalProcDef :
handleNewProcDef :: String ->  A.Node -> [A.Node] -> Proc
handleNewProcDef name procDef paramNodes =
  let paramsAndArgs = map makeParam $ paramNodes
      uniquePaa = (sortOn $ paramvar . fst)
                $ (map head)
                $ (groupBy (\x y ->
                  (paramvar $ fst x) == (paramvar $ fst y)))
                paramsAndArgs
      names = map (paramvar . fst) uniquePaa
      vars = zipWith id (map snd uniquePaa) Asm.argRegs
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
      --newMap = Map.insert name (ProcVar proc) (vars cgs)
      newMap = Map.insert name (ProcVar newProc) (vars cgs)
  in  ((), cgs { vars = newMap })


-- TODO check that the parameters are the same
appendProcDef :: Proc -> A.Node -> [A.Node] -> Proc
appendProcDef proc def paramNodes =
  let params = map (fst . makeParam) paramNodes
  --in  proc { procdefs = (procdefs proc) : (params, def) }
  -- Cancer semnatics
  in  proc { procdefs = (params, def) : (procdefs proc) }

-- This only mutates the CGState and does not write any assembly because the
-- procedure definitions will all happen at the end.
handleProcDef :: [A.Node] -> A.Node -> A.Node -> StateT CGState IO ()
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
  -- [resolve like reference]

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

-- Push argument registers to the stack in preparation for a proc call
-- pushOldArgRegisters ?
pushArgRegisters :: StateT CGState IO ()
pushArgRegisters =
  let
    toStack key var = case varToStore var of
      reg@(Asm.Register _) -> do
        w2f $ Asm.newStackVar
        newStore <- regVarToStack key var
        w2f $ Asm.storeToStore reg newStore
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

procToStore :: CGState -> String -> Asm.Store -> StateT CGState IO ()
procToStore cgs argName targetStore =
  case (vars cgs) Map.! argName of
    (ProcVar Proc{procname=n}) -> w2f $ Asm.procToStore n targetStore
    (ProcVar ProcRef{refstore=rs}) ->
      w2f $ Asm.storeToStore (resolveStore cgs rs) (resolveStore cgs targetStore)
    -- TODO Do I need type safe stores?
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
                  --(ProcVar ProcRef{refstore=rs}, expr) ->
                  --  w2f $ Asm.procToStore (getProcName expr) rs
                  (ProcVar ProcRef{refstore=rs}, expr) ->
                    procToStore cgs (getProcName expr) rs
                    --w2f $ Asm.procToStore (getProcName expr) rs
                  (IntVar store, expr) ->
                    build expr >> (w2f $ Asm.accToStore store)
                 -- This is questionable
  pushArgRegisters
  mapM_ assign args
  updatedCgs <- get
  let updatedProc = case vars updatedCgs Map.! procName of
                      (ProcVar proc) -> proc
  case updatedProc of
    Proc{procname=n} -> w2f $ Asm.callName $ (procprefix cgs) ++ n
    ProcRef{refstore=store} -> w2f $ Asm.callStore (resolveStore updatedCgs store)

makeVal :: A.Node -> CGState -> Asm.Store
makeVal (A.Leaf (A.NameVal _ name)) cgs =
  case getVar cgs name of (Just x) -> x
makeVal (A.Leaf (A.IntVal val)) _ = Asm.Literal val

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


-- needs to take procName as well
-- foldl :: (b -> a -> b) -> b -> t a -> b
-- foldl :: (SCI -> Cond -> SCI) -> SCI -> [Cond] -> SCI
-- foldr to get things in the correct order?
-- Maybe unabstract this?
--buildConds :: CGState -> [Param] -> IO ()
buildConds :: String -> [[Param]] -> StateT CGState IO ()
buildConds procName paramsList = do
  forM_ (zip [0..] $ reverse paramsList) $ \(num, params) -> do
    let condLabel = (procName ++ "c" ++ (show num))
    let procLabel = (procName ++ "d" ++ (show num))
    forM_ (filter (isJust . cond) params) $ \param -> do
      let (ord, node) = fromJust $ cond param
      build node
      cgs <- get
      let store = varToStore $ vars cgs Map.! (paramvar param)
      w2f $ Asm.condJump condLabel store ord
    -- TODO Rename beginProc
    w2f $ Asm.jump procLabel
    w2f $ Asm.beginProc condLabel
  w2f $ Asm.exitWith 2

buildProcs :: CGState -> CGState -> IO ()
buildProcs final init = do
  -- Temporary simplifying assumption: no conds, single def
  --forM_ (Map.keys $ procs final) $ \name -> do
    --forM_ ((procs final) Map.! name) $ \proc -> do
  let nonRefProcs = filterNonRefProcs (vars final)
  forM_ nonRefProcs $ \proc -> do
    let name = procname proc
    let newInit = init { procprefix = (procprefix init) ++ name ++ "_"
                       , vars = justVarMap $ argmap proc
                       }
                       --, procs = filterProcArgs $ fst $ procdefs proc }
                       --, procs = undefined }
    let procName = (procprefix final) ++ name
    (cgwrite init) $ Asm.beginProc procName
    newNewInit <- execStateT (buildConds procName (map fst $ procdefs proc)) newInit
    -- TODO make this accomodate conds
    --forM_ (zip [0..] (procdefs proc)) $ \(num, def) -> do
    --   runStateT (buildConds procName (fst def)) newInit
    --  --buildConds newInit $ fst def
    --  (cgwrite init) $ Asm.beginProc $ procName ++ "c" ++ (show num)
    -- TODO write failure condition
    forM_ (zip [0..] (procdefs proc)) $ \(num, def) -> do
      (cgwrite init) $ Asm.beginProc $ procName ++ "d" ++ (show num)
      buildHelper newNewInit $ snd def
      --buildHelper newInit $ snd def

-- TODO This probably doesn't have to be its own function.
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
                         --, procs = Map.empty
                         , procprefix = ""
                         , sp = 8
                         , cgwrite = (mapM_ (TIO.hPutStr h)) }

