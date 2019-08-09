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
postlude = "\
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
subFromAcc s = [ "    subq  " , showAsm s, ", %rax\n"
               , "    neg   %rax\n" ]

mulAcc :: Store -> [T.Text]
mulAcc s  = [ "    imulq $" , showAsm s, ", %rax\n" ]

push :: Store -> [T.Text]
push s = [ "    push  ", showAsm s, "\n" ]

pushAcc :: [T.Text]
pushAcc = [ "    push  %rax\n" ]

adjustRsp :: Int -> [T.Text]
adjustRsp val = [ "    addq  $" , x, ", %rsp\n" ]
  where x = T.pack $ show val

--exprToStack :: Int -> [T.Text]
--exprToStack offset = [ "    movq  %rax, "
--                     , T.pack $ show offset
--                     , "(%rsp)\n" ]
accToStore :: Store -> [T.Text]
accToStore s = [ "    movq  %rax,", showAsm s, "\n" ]

procToStore :: String -> Store -> [T.Text]
procToStore str sto = [ "    lea   ", T.pack str, "(%rip), ", showAsm sto, "\n" ]

storeToStore :: Store -> Store -> [T.Text]
storeToStore s d = [ "    movq  ", showAsm s, ",", showAsm d, "\n" ]

callName :: String -> [T.Text]
callName name = [ "    call  ", T.pack name,"\n" ]

callStore :: Store -> [T.Text]
callStore s = [ "    call  *", showAsm s,"\n" ]

jumpStore :: Store -> [T.Text]
jumpStore s = [ "    jmp   *", showAsm s,"\n" ]

ordJump :: Ordering -> T.Text
ordJump EQ = "    jne   "
ordJump GT = "    jl    "
ordJump LT = "    jg    "

jump :: String -> [T.Text]
jump to = [ "    jmp   ", T.pack to, "\n" ]

condJump :: String -> Store -> Ordering -> [T.Text]
condJump to sbtr ord = [ "    cmp   ", showAsm sbtr, ", %rax\n"
                       , ordJump ord, T.pack to, "\n" ]

exitWith :: Int -> [T.Text]
exitWith code = [ "    mov   ", T.pack $ show code, ", %rdi\n"
                , "    mov   60, %rax\n"
                , "    syscall\n" ]


makeProc :: String -> [T.Text]
makeProc name = [ T.pack name, ":\n"
                , "    ret\n" ]

beginProc :: String -> [T.Text]
beginProc name = [ "",  T.pack name, ":\n" ]

endProc :: Int -> [T.Text]
endProc val
        | val == 0  = [ "    ret\n" ]
        | otherwise = [ "    add   $", x, ", %rsp\n"
                      , "    ret\n" ]
        where x = T.pack $ show val
