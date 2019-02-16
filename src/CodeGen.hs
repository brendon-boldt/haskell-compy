{-# Language OverloadedStrings #-}
module CodeGen
  ( generateAsm
  ) where

--import Data.HashMap.Strict
import Data.Maybe (isJust, fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad
--import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.State
import System.IO

import Data.Monoid
import Data.Semigroup

import Debug.Trace

import qualified AST as A
import qualified Asm as Asm

data CGState = CGState { vars :: Map.Map String Int
                       , sp :: Int
                       , cgwrite :: [T.Text] -> IO ()}

--w2f :: Handle -> [T.Text] -> IO ()
--w2f h ts = mapM_ (TIO.hPutStr h) ts

initCGState :: Handle -> CGState
--initCGState = CGState Map.empty (-1)
initCGState handle = CGState Map.empty (-1) (mapM_ (TIO.hPutStr handle))

-- CodeGen Monad
type CGM = State CGState



--data W2File = W2File [T.Text -> IO ()]
--data W2File = W2File (IO ())
--
--instance Semigroup W2File where
--  (W2File x) <> (W2File y) = W2File $ x >> y
--instance Monoid W2File where
--  mempty = W2File $ return ()

--cgmPlus :: CGM [a] -> CGM [a] -> CGM [a] 
--cgmPlus x y = liftA2 (++) x y

getOffset :: CGState -> String -> Maybe Int
getOffset cgs var =
  let calc = (\x -> ((sp cgs) - x) * 8)
  in calc <$> (Map.lookup var (vars cgs))

getStackSpace :: CGState -> Int
getStackSpace cgs = (sp cgs + 1) * 8

--newVar :: String -> CGM ()
newVar :: Monad m => String -> StateT CGState m ()
newVar name = state (\cgs ->
  let newSP = sp cgs + 1
      newVarMap = Map.insert name newSP (vars cgs)
  in ((), cgs { vars = newVarMap, sp = newSP }))

--handleLet :: A.Node -> CGM [T.Text]
handleLet :: A.Node -> StateT CGState IO ()
handleLet (A.Leaf (A.NameVal _ name)) = do
  cgs <- get
  let maybeOffset = getOffset cgs name
  if isJust maybeOffset
    then w2f $ Asm.exprToStack (fromJust maybeOffset)
    else newVar name >> (w2f $ Asm.newVar ++ (Asm.exprToStack 0))
    -- 0 because we always know where a new variable is going -- maybe this
    -- will change at some point?

--handleNameExpr :: String -> CGM [T.Text]
--handleNameExpr :: String -> CGM (IO ())
handleNameExpr :: String -> StateT CGState IO ()
handleNameExpr name = do
  cgs <- get
  let maybeOffset = getOffset cgs name
  if isJust maybeOffset
    --then return $ Asm.movVarRax (fromJust maybeOffset)
    then w2f $ Asm.movVarRax (fromJust maybeOffset)
    else undefined

--build :: A.Node -> CGM [T.Text]
build :: A.Node -> StateT CGState IO ()

-- Since Program is effectively a procedure, we need to clean up the stack
-- before we exit.
build (A.Node A.Program cs) = do
  -- TODO do the newline better
  --body <- foldr1 cgmPlus $ map build cs
  --let sep = return ["\n" :: T.Text]
  --body <- concat <$> (sequence [ sep >> (build c) | c <- cs ])
  --sequence [ (build c) | c <- cs ]
  --
  --sequence_ <$> sequence [ (build c) | c <- cs ]
  mapM_ build cs
  cgs <- get
  w2f (Asm.adjustRsp (getStackSpace cgs))

build (A.Node A.ShowExpr (c:[])) = (build c) >> (w2f [Asm.showAsm]) -- why do I have to enlist this?
-- Refactor this into something nicer when I feel like it
build (A.Node A.AddExpr (val:expr:[])) =
  (build expr) >> (w2f $ Asm.addVal val)
--build (A.Node A.SubExpr (val:expr:[])) =
--  (build expr) `cgmPlus` (return $ Asm.subVal val)
--build (A.Node A.MulExpr (val:expr:[])) =
--  (build expr) `cgmPlus` (return $ Asm.mulVal val)
--build (A.Leaf (A.IntExpr val)) = return $ Asm.movValRax val
--build (A.Leaf (A.IntExpr val)) = return $ get >>= (\cgm -> (cgwrite cgm) (Asm.movValRax val))
build (A.Leaf (A.IntExpr val)) =  w2f (Asm.movValRax val)
--build (A.Node A.LetExpr (n:e:[])) = liftA2 (++) (build e) (handleLet n)
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
