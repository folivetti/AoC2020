module Main where

main :: IO ()
main = do 
  let crack x = fst $ until ((==x).snd) (\(i,n) -> (i+1, n*7 `rem` 20201227)) (0,1)
  print $ iterate (\val -> val*8335663 `rem` 20201227) 1 !! crack 8614349
