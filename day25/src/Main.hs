{-# language DataKinds #-}
module Main where

import System.Environment
import Data.Maybe
import Math.NumberTheory.Moduli hiding (powMod)
import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.Powers (powMod)

bruteForce = 
  let crack x = fst $ until ((==x).snd) (\(i,n) -> (i+1, n*7 `rem` 20201227)) (0,1)
  in  iterate (\val -> val*8335663 `rem` 20201227) 1 !! crack 8614349

fast = 
  let cg = fromJust cyclicGroup :: CyclicGroup Integer 20201227
      rt = fromJust (isPrimitiveRoot cg 7)
      x  = fromJust (isMultElement 8614349)
      loop = discreteLogarithm cg rt x
  in  powMod 8335663 loop 20201227

main :: IO ()
main = do
  args <- getArgs
  case args of
       []        -> error "Usage: stack run {brute|fast}"
       ["fast"]  -> print fast
       ["brute"] -> print bruteForce
