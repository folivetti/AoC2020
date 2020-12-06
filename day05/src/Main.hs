{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char      (digitToInt)
import Numeric        (readInt)
import Data.List      (sort)
import Data.Maybe     (catMaybes)

import Data.IntSet    (fromList, IntSet(), member)


import Data.Monoid
import Data.Semigroup
import Prelude hiding (sum, max, min)
import Data.Foldable (foldl')

-- from https://github.com/Gabriel439/slides/blob/master/munihac/foldmap.md
data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)

fold' :: Fold i o -> [i] -> o
fold' (Fold toMonoid summarize) is = summarize folded
  where
    folded = foldl' (<>) mempty $ map toMonoid is

data Pair a b = P !a !b 
              deriving Show 

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (<>) (P aL bL) (P aR bR) = P (aL <> aR) (bL <> bR)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = P mempty mempty

instance Functor (Fold i) where
  fmap f (Fold g h) = Fold g (f.h)

(<+>) :: Fold i a -> Fold i b -> Fold i (Pair a b)
(<+>) (Fold mL sL) (Fold mR sR) = Fold toMonoid summarize
  where
    toMonoid x        = P (mL x) (mR x)
    summarize (P x y) = P (sL x) (sR y)


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

sum, max, min :: (Ord n, Num n, Bounded n) => Fold n n
sum = Fold Sum getSum
max = Fold Max getMax
min = Fold Min getMin

-- | stolen from https://github.com/francesquini/AoC2020/blob/main/src/Problem05.hs
pa :: Fold Int Int
pa = diff <$> (sum <+> (sumPA <$> (max <+> min)))
  where
    diff (P sumvals total) = total - sumvals
    sumPA (P mx mn)        = (mx + mn) * (mx - mn + 1) `div` 2

main :: IO ()
main = do
  seats <- map getId . lines <$> readFile "day05.txt"
  let setSeats = fromList seats
  print $ maximum seats
  print $ findId seats
  print $ findId' $ sort seats
  print $ withSet seats setSeats
  print $ fold' pa seats
