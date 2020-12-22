module Main where

import Data.Bifunctor
import Data.Set (member, insert)
import qualified Data.Set as S

data Player = P1 | P2

play :: Ord a => [a] -> [a] -> [a]
play [] p2 = p2
play p1 [] = p1
play (x:xs) (y:ys)
  | x > y     = play (xs ++ [x,y]) ys
  | x < y     = play xs (ys ++ [y,x])
  | otherwise = play (xs ++ [x]) (ys ++ [y])

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

play' :: [Int] -> [Int] -> Either [Int] [Int]
play' p1 p2 = go p1 p2 S.empty
  where
    go [] p2 _ = Right p2
    go p1 [] _ = Left p1
    go p1@(x:xs) p2@(y:ys) s
      | (p1,p2) `member` s = Left xs
      | otherwise          = case winner p1 p2 of
                                  P1 -> go (xs ++ [x,y]) ys (insert (p1,p2) s)
                                  P2 -> go xs (ys ++ [y,x]) (insert (p1,p2) s)
    winner (x:xs) (y:ys)
      | x <= length xs && y <= length ys = case play' (take x xs) (take y ys) of
                                                Left  _ -> P1
                                                Right _ -> P2
      | x > y                            = P1 
      | x < y                            = P2

p1' = [9 ,2 ,6 ,3 ,1]
p2' = [5 ,8 ,4 ,7 ,10]

main :: IO ()
main = do
  dat <- lines <$> readFile "day22.txt"
  let p1 = map read $ take 25 $ tail dat :: [Int]
      p2 = map read $ drop 28 dat :: [Int]
      win = play p1 p2
      win2 = play' p1 p2
  print $ score win
  print $ bimap score score win2
