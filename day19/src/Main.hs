module Main where

import Data.Either     (fromRight)
import Data.Bifunctor  (first)
import Data.Map.Strict (Map(..),fromList, (!), insert)
import Text.Parsec

data Rule = Single Int | And Rule Rule | Or Rule Rule | Token Char
          deriving Show

-- * Parsing stuff
getToken :: Parsec String () Rule
getToken = Token <$> between (char '\"') (char '\"') letter

decimal :: (Read a, Integral a) => Parsec String () a
decimal = read <$> many digit

getSingle :: Parsec String () Rule
getSingle = Single <$> decimal 

getAnd :: Parsec String () Rule
getAnd = And <$> getSingle <*> (char ' ' *> getSingle)

getOr :: Parsec String () Rule
getOr = try (Or <$> getSingle <*> (string " | " *> getSingle)) 
     <|>    (Or <$> getAnd    <*> (string " | " *> getAnd))

getKey :: Parsec String () Int
getKey = between (string "") (string ": ") decimal

getRule :: Parsec String () Rule
getRule = try getToken <|> try getOr <|> try getAnd <|> getSingle

getData :: Parsec String () (Int, Rule)
getData = (,) <$> getKey <*> getRule

parseMap :: [String] -> Map Int Rule
parseMap = fromList . map (fromRight (-1, Single 0) . parse getData "") 

-- * Non-deterministic finite automata
runNDFA :: Map Int Rule -> Rule -> String -> [String]
runNDFA rules _          ""      = []
runNDFA rules (Token c)  (c':cs) = [cs | c==c']
runNDFA rules (Single x)  cs     = runNDFA rules (rules ! x) cs
runNDFA rules (And r1 r2) cs     = runNDFA rules r1 cs >>= runNDFA rules r2
runNDFA rules (Or  r1 r2) cs     = runNDFA rules r1 cs ++ runNDFA rules r2 cs

r0  = Single 0
r8  = Or (Single 42) (And (Single 42) (Single 8))
r11 = Or (And (Single 42) (Single 31)) (And (And (Single 42) (Single 11)) (Single 31))

main :: IO ()
main = do
  dat <- lines <$> readFile "day19.txt"
  let (rules, strings) = first parseMap . span (/="") $ dat
      rules'           = insert 8 r8 . insert 11 r11   $ rules
      countWith r      = length . filter (elem "" . runNDFA r r0) $ strings
  print $ countWith rules
  print $ countWith rules'
