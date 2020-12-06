module Main where

import Data.Char      (digitToInt)
import Numeric        (readInt)
import Data.List      (sort)
import Data.Maybe     (catMaybes)

getId :: String -> Int
getId = fst . head . readInt 2 (`elem` "01") digitToInt . map parser
  where
    parser 'F' = '0'
    parser 'B' = '1'
    parser 'R' = '1'
    parser 'L' = '0'

findId :: [Int] -> Maybe Int
findId = go . sort
  where
    go []         = Nothing 
    go (x:y:xs)
      | y == x+2  = Just $ x+1
      | otherwise = go (y:xs)

findId' :: [Int] -> Int
findId' seats = head 
              $ catMaybes 
              $ zipWith (\x y -> if y==x+2 then Just (x+1) else Nothing) seats (tail seats)

main :: IO ()
main = do
  seats <- map getId . lines <$> readFile "day05.txt"
  print $ maximum seats
  print $ findId seats
  print $ findId' $ sort seats
