module Main where

import Data.List.Split
import Data.Char
import Data.List
import Data.IntMap.Strict (IntMap(..))
import qualified Data.IntMap.Strict as M

parseTiles :: [String] -> (Int, [String])
parseTiles (t:ts) = let key = parseKey t
                    in  (key, ts)
  where
    parseKey = read . takeWhile isDigit . dropWhile (not.isDigit)

printTile :: [String] -> IO ()
printTile = putStrLn . unlines

-- Part 1
toBorders :: [String] -> [String]
toBorders css = map (\b -> min b (reverse b)) [head css, last css, map head css, map last css]

count :: IntMap [String] -> [String] -> Int
count borders b = length $ filter null $ map matches b
  where
    keepMatches b' = M.filter (\bi -> bi /= b && b' `elem` bi) borders
    matches        = M.keys . keepMatches

-- part 2
vertRotation :: [a] -> [[a]]
vertRotation board = [board, reverse board]

allRotations :: [[a]] -> [[[a]]]
allRotations board = vertRotation board >>= horizontal
  where
    horizontal = take 4 . iterate (transpose . reverse)

mergeNext :: [String] -> [String] -> [String]
mergeNext xs ys = safeHead merged 
  where
    safeHead []     = []
    safeHead (xs:_) = xs

    reorder x y = reverse (tail x) ++ tail y    
    merged      = [reorder x y | x  <- vertRotation xs
                               , y  <- allRotations ys
                               , head x == head y]

mergeColumn :: [[String]] -> [[String]]
mergeColumn (b:bs) = go bs []
  where
    go [] xs     = reverse (b:xs)
    go (y:ys) xs = case mergeNext b y of
                        []  -> go ys (y:xs)
                        z   -> mergeColumn (z : (reverse xs ++ ys))

merge :: [[String]] -> [String]
merge [board] = removeBorder board
merge boards  = let boards' = mergeColumn boards  -- try to add a new column
                in  if length boards == length boards'  -- if we don't succeed
                    then merge . map transpose $ boards' -- transpose and add a new row 
                    else merge boards'                   -- else, keep inserting columns

removeUpBottom = init.tail
removeBorder   = map removeUpBottom . removeUpBottom

-- monster detection system 
hasMonster :: [String] -> Int
hasMonster []     = 0
hasMonster ("":_) = 0
hasMonster grid
  | isMonster grid = hasMonster (map tail grid) + 1
  | otherwise      = hasMonster (map tail grid)

isMonster :: [String] -> Bool
isMonster (top:mid:bot:_) = length top >= 20 && all (=='#') pat
  where
    pat = concat $ zipWith (\a b -> map (a!!) b) [top,mid,bot] [[18], [0,5,6,11,12,17,18,19], [1,4,7,10,13,16]]
isMonster _ = False

countInThisRot :: [String] -> Int
countInThisRot = sum . map hasMonster . tails

countMonsters :: [String] -> Int
countMonsters = maximum . map countInThisRot . allRotations

countNotMonster :: [String] -> Int
countNotMonster grid = nSquares - 15 * countMonsters grid
  where
    nSquares  = foldr acc 0 (concat grid)
    acc '#' x = x+1
    acc  _  x = x

main :: IO ()
main = do
  dat <- map parseTiles . splitOn [""] . lines <$> readFile "day20.txt"
  let tiles   = M.fromList dat
      borders = M.map toBorders tiles
      part1   = product $ M.keys $ M.filter (==2) $ M.map (count borders) borders

      boards  = map snd dat
      board   = merge boards
  print part1
  print $ countNotMonster board
