{-# LANGUAGE OverloadedStrings #-}
module Asm
  ( generateAsm
  ) where

import Data.Text
import qualified Data.Text.IO as TIO
--import Data.HashMap.Strict
import Data.Map.Strict

import qualified AST as A

preamble :: Text
preamble = ".global   main\n\
           \.text\n\n\
           \main:\n"
           -- \    mov   $4, %rsi\n\
           -- \    call  show_int\n\n"

postlude :: Text
postlude = "\n\
           \    xor   %rax, %rax\n\
           \    ret\n"



generateAsm :: A.Node -> IO ()
generateAsm ast = do
  let fn = "asm/main.s"
  TIO.appendFile fn preamble
  TIO.appendFile fn postlude
