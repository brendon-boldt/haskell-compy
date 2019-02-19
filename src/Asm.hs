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

showAsm :: [T.Text]
showAsm = ["\
          \    movq  %rax, %rsi\n\
          \    call  show_int\n"]

movValRax :: Int -> [T.Text]
movValRax val = [ "    movq  $"
                , T.pack $ show val
                , ", %rax\n" ]

movVarRax :: Int -> [T.Text]
movVarRax offset = [ "    movq  "
                   , T.pack $ show offset
                   , "(%rsp), %rax\n" ]

makeNameVal :: Int -> T.Text
makeNameVal offset = (T.pack $ show offset) `T.append` "(%rsp)"

makeIntVal :: Int -> T.Text
makeIntVal val = "$" `T.append` (T.pack $ show val)

addVal :: T.Text -> [T.Text]
addVal val = [ "    add   " , val, ", %rax\n" ]

subVal :: T.Text -> [T.Text]
subVal val = [ "    sub   $" , val, ", %rax\n" 
             , "    neg   %rax\n" ]

mulVal :: T.Text -> [T.Text]
mulVal val  = [ "    imul  $" , val, ", %rax\n" ]

newVar :: [T.Text]
newVar = [ "    sub   $8, %rsp\n" ]

adjustRsp :: Int -> [T.Text]
adjustRsp val = [ "\n    add   $" , x, ", %rsp\n" ]
  where x = T.pack $ show val

exprToStack :: Int -> [T.Text]
exprToStack offset = [ "    movq  %rax, "
                     , T.pack $ show offset
                     , "(%rsp)\n" ]

callName :: String -> [T.Text]
callName name = [ "    call  ", T.pack name,"\n" ]

makeProc :: String -> [T.Text]
makeProc name = [ T.pack name, ":\n"
                , "    ret\n" ]

beginProc :: String -> [T.Text]
beginProc name = [ "\n",  T.pack name, ":\n" ]

endProc :: Int -> [T.Text]
endProc val = [ "    add   $", x, ", %rsp\n"
              , "    ret\n" ]
                where x = T.pack $ show val
