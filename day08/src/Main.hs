module Main where

import Data.Char (toUpper)
import Data.Array
import Data.IntSet (member, IntSet(), insert, empty)

import Algebra

type Code = Array Int OP

data OP = NOP Int | ACC Int | JMP Int
        deriving (Show, Read)

codeSequence ::  Code -> CoAlgebra (ListF (Int, Int)) (Int, Int)
codeSequence code (loc, acc) 
  | loc > snd (bounds code) = NilF
  | otherwise = ConsF (loc, acc) next 
  where
    next = case code ! loc of
              NOP _ -> (loc+1, 0)
              ACC x -> (loc+1, x)
              JMP x -> (loc+x, 0)

runCode :: Algebra (ListF (Int, Int)) Int
runCode NilF        = 0
runCode (ConsF e n) = snd e + n

getTrace :: Algebra (ListF (Int, Int)) [(Int, Int)]
getTrace NilF        = []
getTrace (ConsF e n) = e : n

takeBeforeDup :: Algebra (ListF (Int, Int)) (IntSet -> Int)
takeBeforeDup NilF                 = const 0
takeBeforeDup (ConsF (loc, acc) n) = fs
  where
    fs set
      | loc `member` set = 0
      | otherwise        = acc + n (insert loc set)

hyloFindDup :: Code -> Int
hyloFindDup code = hylo takeBeforeDup (codeSequence code) (0,0) empty

doesItTerminate :: Algebra (ListF (Int, Int)) (IntSet -> (Bool, Int))
doesItTerminate NilF                 = const (True, 0)
doesItTerminate (ConsF (loc, acc) n) = fs
  where
    fs set
      | loc `member` set = (False, 0)
      | otherwise        = (+acc) <$> n (insert loc set)

hyloTerminate :: Code -> (Bool, Int)
hyloTerminate code = hylo doesItTerminate (codeSequence code) (0,0) empty

findFix :: Array Int OP -> Int
findFix code = go code lo
  where
    (lo, hi) = bounds code
    changeAt c x = case c ! x of
                        ACC _ -> (True , code)
                        NOP n -> (False, code // [(x,JMP n)])
                        JMP n -> (False, code // [(x,NOP n)])

    go code x
      | x > hi    = error "no fix found"
      | isAcc     = go code (x+1)
      | term      = acc
      | otherwise = go code (x+1)
      where
        (isAcc, code') = code `changeAt` x
        (term, acc)    = hyloTerminate code'

main :: IO ()
main = do
  contents <- lines . filter (/='+') . map toUpper <$> readFile "day08.txt"
  let 
    ops     = map read contents :: [OP]
    n       = length ops
    code    = array (0,n-1) $ zip [0..] ops
  print $ hyloFindDup code
  print $ findFix code
