module Main where

import Data.Bool
import Data.List
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

reduceM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
reduceM x xs f  = foldM f x xs
reduceM_ x xs f = foldM_ f x xs

runPart2 :: Int -> [Int] -> Int
runPart2 turns xs =
  let n = maximum xs
  in runST $ do
    arr <- newArray (1, n) (-1) :: ST s (STUArray s Int Int)
    foldM_ (\x y -> do writeArray arr x y; return y) (head xs) (tail xs)
    writeArray arr (last xs) (head xs)
    reduceM_ (head xs) [1..turns]
      (\cur _ -> do x1 <- readArray arr cur
                    x2 <- readArray arr x1
                    x3 <- readArray arr x2
                    x4 <- readArray arr x3
                    let ix = getIndexMax n (cur-1) [cur,x1,x2,x3]
                    x  <- readArray arr ix
                    writeArray arr cur x4
                    writeArray arr x3 x
                    writeArray arr ix x1
                    return x4) 
    x1 <- readArray arr 1
    x2 <- readArray arr x1
    return (x1*x2)

getIndexMax :: (Eq a, Num a, Foldable t) => a -> a -> t a -> a
getIndexMax maxN 0 xs = getIndexMax maxN maxN xs
getIndexMax maxN x xs = bool x (getIndexMax maxN (x-1) xs) (x `elem` xs)

data Zipper a = Zip [a] [a] deriving Show

toZipper :: [a] -> Zipper a
toZipper [] = Zip [] []
toZipper xs = Zip [] xs

fromZipper :: Zipper a -> [a]
fromZipper (Zip [] []) = []
fromZipper (Zip ys xs) = xs ++ reverse ys

answer :: (Num a, Eq a) => [a] -> [a]
answer xs = case 1 `elemIndex` xs of
                 Nothing -> error "wrong answer"
                 Just n  -> drop (n+1) xs ++ take n xs

move :: (Num a, Eq a, Show a) => Zipper a -> Zipper a
move (Zip ys (x:xs)) =
  let (cur, xs') = splitAt 3 xs
      ix         = getIndex (x-1) (x:cur)
  in case ix `elemIndex` xs' of
          Nothing  -> Zip [x]    $ concatAt ix cur (xs' ++ reverse ys)
          Just n   -> Zip (x:ys) $ take (n+1) xs' ++ cur ++ drop (n+1) xs'


concatAt ix xs ys =
  case ix `elemIndex` ys of
       Nothing -> error $ "that cannot happen" ++ show ix ++ " " ++ show xs ++ " " ++ show ys
       Just n  -> take (n+1) ys ++ xs ++ drop (n+1) ys

getIndex :: (Eq a, Num a, Foldable t) => a -> t a -> a
getIndex 0 xs = getIndex 9 xs
getIndex 1 xs = bool 1 (getIndex 9 xs)     (1 `elem` xs)
getIndex x xs = bool x (getIndex (x-1) xs) (x `elem` xs)

example = [3,8,9,1,2,5,4,6,7]
input   = [2,4,7,8,1,9,3,5,6]
input2  = input ++ [10..1000000]

main :: IO ()
main = do
  let play  = fromZipper <$> iterate move (toZipper input)
      part1 = answer (play !! 100)
      play2 = runPart2 10000000 input2
  putStrLn $ concatMap show part1
  print play2
