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
toBorders css = map hashOrientation [head css, last css, map head css, map last css]
  where
    hashOrientation b = min b (reverse b)

-- count how many coincident hashed borders we have    
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
    horizontal xs = map ($xs) [id, transpose.reverse, reverse.map reverse, transpose.map reverse]

-- try to merge some block from ys on top or bottom of xs
-- if it fails it returns []
mergeNext :: [String] -> [String] -> [String]
mergeNext xs ys = safeHead merged 
  where
    safeHead []     = []
    safeHead (xs:_) = xs

    reorder x y = reverse (tail x) ++ tail y    -- remove the connecting borders
    merged      = [reorder x y | x  <- vertRotation xs -- check if matches the top or bottom 
                               , y  <- allRotations ys -- rotate ys at will 
                               , head x == head y]     -- it matches 

-- creates a single column     
mergeColumn :: [[String]] -> [[String]]
mergeColumn (b:bs) = go bs []
  where
    go [] xs     = reverse (b:xs)         -- restore the order of the stack 
    go (y:ys) xs = case mergeNext b y of  -- try to stack y in b 
                        []  -> go ys (y:xs) -- if it doesn't succeed, try the next but keep y in the unused stack 
                        z   -> mergeColumn (z : (reverse xs ++ ys)) -- if it does, restore the unused stack and call it again

merge :: [[String]] -> [String]
merge [board] = board
merge boards
  | length boards == length boards' = merge . map transpose $ boards' -- no more columns, merge rows 
  | otherwise                       = merge boards'                   -- keep inserting columns
  where
    boards' = mergeColumn boards -- try to create a new column 

removeUpBottom :: [a] -> [a]
removeUpBottom = init.tail

removeBorder :: [[a]] -> [[a]]
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
      board   = removeBorder $ merge boards
  print part1
  print $ countNotMonster board
