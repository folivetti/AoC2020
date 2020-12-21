{-# language TupleSections #-}
module Main where

import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

example = ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
          ,"trh fvjkl sbzzf mxmxvkd (contains dairy)"
          ,"sqjhc fvjkl (contains soy)"
          ,"sqjhc mxmxvkd sbzzf (contains fish)"
          ]

parse :: String -> ([String], [String])
parse css = let ws          = words css
                ingredients = takeWhile (/="(contains") ws
                alergens    = map (filter (not . (`elem` ",)"))) $ tail $ dropWhile (/="(contains") ws
            in  (ingredients, alergens)

toMap :: [String] -> [String] -> M.Map String (S.Set String)
toMap ingredients alergens = let is = S.fromList ingredients
                             in  M.fromList $ map (,is) alergens

mergeMaps :: [M.Map String (S.Set String)] -> M.Map String (S.Set String)
mergeMaps = M.unionsWith S.intersection

findAlergens :: M.Map String (S.Set String) -> M.Map String String
findAlergens m
  | all isSingleton (M.elems m) = M.map (head . S.toList) m
  | otherwise                   = findAlergens (step m)

isSingleton :: S.Set a -> Bool
isSingleton x = S.size x == 1

notMember :: Ord a => a -> S.Set a -> Bool
notMember x s = not (x `S.member` s)

step :: M.Map String (S.Set String) -> M.Map String (S.Set String)
step m = let singletons    = M.foldr unionIf S.empty m
             unionIf s1 s2 = if isSingleton s1 then S.union s1 s2 else s2
             diffIf  s     = if not (isSingleton s) then S.difference s singletons else s
         in  M.map diffIf m

main :: IO ()
main = do
  dat <- lines <$> readFile "day21.txt"
  let recipes      = map parse dat
      alergens     = findAlergens . mergeMaps $ map (uncurry toMap) recipes
      withAlergens = S.fromList $ M.elems alergens
      noAlergens   = filter (`notMember` withAlergens) $ concatMap fst recipes
  print $ length noAlergens
  print $ intercalate "," . map snd $ M.toAscList alergens
