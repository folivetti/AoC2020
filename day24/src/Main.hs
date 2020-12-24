{-# language BangPatterns #-}
module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Bool

data Direction = E | SE | SW | W | NW | NE
type Coord = (Int, Int, Int)
type Grid = S.Set Coord

move E  (x, y, z) = (x+1, y-1,   z)
move SE (x, y, z) = (x,   y-1, z+1)
move SW (x, y, z) = (x-1,   y, z+1)
move W  (x, y, z) = (x-1, y+1,   z)
move NW (x, y, z) = (x,   y+1, z-1)
move NE (x, y, z) = (x+1,   y, z-1)

deltas = filter (/=(0,0,0)) $ [(x,y,z) | x <- [-1..1]
                                       , y <- [-1..1]
                                       , z <- [-1..1]
                                       , x+y+z == 0]
getCoord :: String -> Coord
getCoord = go (0,0,0)
  where
    go coord ""           = coord
    go coord ('s':'e':cs) = go (move SE coord) cs
    go coord ('s':'w':cs) = go (move SW coord) cs
    go coord ('n':'w':cs) = go (move NW coord) cs
    go coord ('n':'e':cs) = go (move NE coord) cs
    go coord ('w':cs)     = go (move W coord) cs
    go coord ('e':cs)     = go (move E coord) cs

parse :: [String] -> Grid
parse = go S.empty 
  where
    go s []     = s
    go s (c:cs) = let coord = getCoord c
                  in  bool (go (S.insert coord s) cs) (go (S.delete coord s) cs) (S.member coord s)

update :: [Coord] -> Grid -> Grid
update ds s = S.union s1 s2
  where
    ns          = neighbors ds s
    isActive xs = M.keysSet . M.filter (`elem` xs)
    -- active neighborhood
    s1 = isActive [1,2] $ M.restrictKeys ns s
    -- inactive neighborhood
    s2 = isActive [2]   $ M.withoutKeys ns s

-- returns a map counting the number of active neighbors for each
-- active coordinate and their neighbors
neighbors :: [Coord] -> Grid -> M.Map Coord Int
neighbors ds s = M.unionsWith (+) 
               $ map (forKeys m . sumCoord) ds -- each map has a coord as key and the value is the number of active neighbors
  where
    forKeys = flip M.mapKeys        -- for each key (coord) from a map, apply a function
    m       = M.fromSet (const 1) s -- every active cell has value 1

sumCoord :: Coord -> Coord -> Coord
sumCoord (x,y,z) (a,b,c) = (x+a,y+b,z+c)

main :: IO ()
main = do
  dat <- lines <$> readFile "day24.txt"
  let coords = parse dat
      part1  = S.size coords
      part2  = S.size $ iterate (update deltas) coords !! 100
  print part1
  print part2
