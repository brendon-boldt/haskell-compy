{-# LANGUAGE OverloadedStrings #-}
module Asm
  ( generateAsm
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
--import Data.HashMap.Strict
--import Data.Map.Strict as MS

import qualified AST as A

preamble :: T.Text
preamble = ".global   main\n\
           \.text\n\n\
           \main:\n"
           -- \    mov   $4, %rsi\n\
           -- \    call  show_int\n\n"

postlude :: T.Text
postlude = "\n\
           \    xor   %rax, %rax\n\
           \    ret\n"

showAsm :: T.Text
showAsm = "\n\
          \    movq  %rax, %rsi\n\
          \    call  show_int\n"

movValRax :: Int -> [T.Text]
movValRax val = [ "    movq  $" :: T.Text
                , T.pack $ show val
                , ", %rax\n" :: T.Text ]

addVal :: A.Node -> [T.Text]
addVal (A.Leaf (A.IntVal val)) = [ "    add  $"
                                 , T.pack $ show val
                                 , ", %rax\n" ]


--buildExpr :: A.Node -> [T.Text]
--buildExpr (A.Leaf (A.IntVal val)) = movValRax val
--buildExpr (A.Node A.AddExpr (val:expr:[])) = (buildExpr expr) ++ (addVal val)

-- TODO Should I build everything in reverse?
build :: A.Node -> [T.Text]

build (A.Node A.Program cs) = concatMap build cs
--build (A.Node A.ShowExpr (c:[])) = (build c) ++ [showAsm]
build (A.Node A.ShowExpr (c:[])) = (build c) ++ [showAsm]
build (A.Node A.AddExpr (val:expr:[])) = (build expr) ++ (addVal val)
build (A.Leaf (A.IntExpr val)) = movValRax val

generateAsm :: A.Node -> IO ()
generateAsm ast = do
  let fn = "asm/main.s"
  let program = build ast
  mapM_ (TIO.appendFile fn) (preamble : program)
  TIO.appendFile fn postlude
