{-# LANGUAGE OverloadedStrings #-}
module Asm
  ( module Asm
  ) where

import qualified Data.Text as T
--import Data.HashMap.Strict
--import Data.Map.Strict as MS

--import qualified AST as A

class ShowAsm a where
  showAsm :: a -> T.Text

data Store = Register T.Text | Stack Int | Literal Int
  deriving Show

instance ShowAsm Store where
  showAsm (Register reg) = T.concat ["%", reg]
  showAsm (Stack offset) = T.concat [T.pack $ show offset, "(%rsp)"]
  showAsm (Literal val) = T.concat ["$" :: T.Text, T.pack $ show val]

argRegs = map Register [ "rdi", "rsi", "rdx", "rcx", "r8", "r9" ]
accReg = Register "rax"

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

showAcc :: [T.Text]
showAcc = [ "    movq  %rax, ", showAsm $ head argRegs, "\n" 
          , "    call  show_int\n" ]

-- Eventually these could be monadic actions, right?
movToAcc :: Store -> [T.Text]
movToAcc s = [ "    movq  ", showAsm s, ", ", showAsm accReg, "\n" ]

--makeNameVal :: Int -> T.Text
--makeNameVal offset = (T.pack $ show offset) `T.append` "(%rsp)"

--makeIntVal :: Int -> T.Text
--makeIntVal val = "$" `T.append` (T.pack $ show val)

addToAcc :: Store -> [T.Text]
addToAcc s = [ "    addq  " , showAsm s, ", %rax\n" ]

subFromAcc :: Store -> [T.Text]
subFromAcc s = [ "    subq  $" , showAsm s, ", %rax\n" 
               , "    neg   %rax\n" ]

mulAcc :: Store -> [T.Text]
mulAcc s  = [ "    imulq $" , showAsm s, ", %rax\n" ]

newVar :: [T.Text]
newVar = [ "    subq  $8, %rsp\n" ]

adjustRsp :: Int -> [T.Text]
adjustRsp val = [ "\n    addq  $" , x, ", %rsp\n" ]
  where x = T.pack $ show val

--exprToStack :: Int -> [T.Text]
--exprToStack offset = [ "    movq  %rax, "
--                     , T.pack $ show offset
--                     , "(%rsp)\n" ]
accToStore :: Store -> [T.Text]
accToStore s = [ "    movq  %rax,", showAsm s, "\n" ]

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
