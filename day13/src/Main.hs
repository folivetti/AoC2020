module Main where

import Data.List
import Data.Ord
import Data.List.Split
import Data.Bifunctor

example = "939\n7,13,x,x,59,x,31,19"

parse :: String -> [(Integer, Integer)]
parse = map (second read) . filter ((/="x").snd) . zip [0..] . splitOn ","

parseData :: String -> (Integer, [(Integer, Integer)])
parseData dat = let (x:y:_) = lines dat
                in  (read x, parse y)

sieve :: [(Integer, Integer)] -> Integer
sieve = head . snd . foldl' go (1, [0])
  where
    createSeeds mult x  = map (\i -> x + i*mult) [0..] -- [x, x+mult..]
    filterSeeds n a     = head . filter (\i -> (i+a) `mod` n == 0) 
    nextSeeds m n a     = createSeeds (m*n) . filterSeeds n a -- map.filter 

    go (m, seeds) (a,n) = (m*n, nextSeeds m n a seeds) -- swap

sieve' :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
sieve' (seed, mul) (offset, bus) = (smallestSolution, bus*mul)
  where
    smallestSolution = head [t | t <- [seed, seed+mul ..]
                               , (t+offset) `rem` bus == 0]

main :: IO ()
main = do
  dat <- readFile "day13.txt"
  let (n, offsBuses) = parseData dat -- example
      buses          = map snd offsBuses
      turns          = map (\bus -> bus - (n `mod` bus)) buses
      (tf, bus)      = minimumBy (comparing fst) $ zip turns buses
  print $ tf*bus
  print $ sieve offsBuses
  print $ foldl' sieve' (0, 1) offsBuses
