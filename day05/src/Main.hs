module Main where

import Data.Char      (digitToInt)
import Numeric        (readInt)
import Data.List      (sort)
import Data.Maybe     (catMaybes)

import Data.IntSet    (fromList, IntSet(), member)

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

withSet :: [Int] -> IntSet -> Maybe Int
withSet [] ref  = Nothing
withSet (x:xs) ref
  | isMissing = Just (x+1)
  | otherwise = withSet xs ref
  where
    isMissing = (x+2) `member` ref
              && not ( (x+1) `member` ref)
    
main :: IO ()
main = do
  seats <- map getId . lines <$> readFile "day05.txt"
  let setSeats = fromList seats
  print $ maximum seats
  print $ findId seats
  print $ findId' $ sort seats
  print $ withSet seats setSeats
