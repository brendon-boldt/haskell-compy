{-# LANGUAGE OverloadedStrings #-}
module Asm
  ( module Asm
  ) where

import qualified Data.Text as T
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

movVarRax :: Int -> [T.Text]
movVarRax offset = [ "    movq  "
                   , T.pack $ show offset
                   , "(%rsp), %rax\n" ]

addVal :: A.Node -> [T.Text]
addVal (A.Leaf (A.IntVal val)) = [ "    add   $"
                                 , T.pack $ show val
                                 , ", %rax\n" ]

subVal :: A.Node -> [T.Text]
subVal (A.Leaf (A.IntVal val)) = [ "    sub   $"
                                 , T.pack $ show val
                                 , ", %rax\n" 
                                 , "    neg   %rax\n" ]

mulVal :: A.Node -> [T.Text]
mulVal (A.Leaf (A.IntVal val)) = [ "    imul  $"
                                 , T.pack $ show val
                                 , ", %rax\n" ]

newVar :: [T.Text]
newVar = [ "    sub   $8, %rsp\n" ]

adjustRsp :: Int -> [T.Text]
adjustRsp val = [ "    add   $"
                , T.pack $ show val
                , ", %rsp" ]

exprToStack :: Int -> [T.Text]
exprToStack offset = [ "    movq  %rax, "
                     , T.pack $ show offset
                     , "(%rsp)\n" ]

