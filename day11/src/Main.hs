module Main where

import Data.Array
import Control.Comonad

data Seats = Floor | Empty | Occupied
           deriving (Eq, Show)

data Board a = Board { focus :: (Int, Int)          -- coordenada do foco atual
                     , board :: Array (Int, Int) a  -- dados que contém os focos
                     } 

instance Functor Board where
  fmap f (Board fc b) = Board fc (fmap f b)

instance Comonad Board where
  extract (Board ix b)  = b ! ix
  extend f (Board ix b) = Board ix b'
    where
      g i = (i, f (Board i b))
      b'  = array (bounds b) $ map g (indices b)

within :: Ord a => (a,a) -> ((a, a), (a,a)) -> Bool
within (x,y) ((a,b), (c,d)) = x >= a && y >= b && x <= c && y <= d 

-- walk torward a given direction
to :: (Int -> Int) -> (Int -> Int) -> Board a -> Maybe (Board a)
to f g (Board (x,y) b)
  | c' `within` bounds b = Just $ Board c' b
  | otherwise            = Nothing
  where
    c' = (f x, g y)

-- auxiliary functions for needed direction
stay, right, left, up, down :: Board a -> Maybe (Board a)
stay  = to id id
right = to id (+1)
left  = to id (subtract 1)
up    = to (subtract 1) id
down  = to (+1) id

upright, upleft, downright, downleft :: Board a -> Maybe (Board a)
upright   = to (subtract 1) (+1)
upleft    = to (subtract 1) (subtract 1)
downright = to (+1) (+1)
downleft  = to (+1) (subtract 1)

-- generate the local view given a sight function
neighbor :: ((Board a -> Maybe (Board a)) -> t -> b) -> t -> [[b]]
neighbor sight b = map (map (`sight` b)) neighs
  where
    neighs = [[upleft,   up,   upright]
             ,[left,     stay, right]
             ,[downleft, down, downright]]

-- state of the adjacent spaces
immediate, first :: (Board Seats -> Maybe (Board Seats)) -> Board Seats -> Seats
immediate f b = maybe Empty extract (f b)

-- state of the first seat in the line of sight
first f b = case f b of
                 Just b' -> if extract b' == Floor && focus b /= focus b'
                               then first f b'
                               else extract b'
                 Nothing -> Empty

-- occupy the seat if it's empty and no other occupied around
-- leave the seat if there are more than maxSeats occupied around
changeState :: Int -> [[Seats]] -> Seats
changeState _        [[s1, s2,    s3], 
                      [s4, Empty, s6], 
                      [s7, s8,    s9]]    = leaveIf $ Occupied `elem` [s1,s2,s3,s4,s6,s7,s8,s9]
changeState maxSeats [[s1, s2,       s3], 
                      [s4, Occupied, s6], 
                      [s7, s8,       s9]] = leaveIf $ length (filter (==Occupied) [s1,s2,s3,s4,s6,s7,s8,s9]) >= maxSeats

changeState _        [_ , [_, s, _], _] = s

-- leave the seat if true, else occupy the seat
leaveIf :: Bool -> Seats
leaveIf True  = Empty
leaveIf False = Occupied

-- parse symbol into Seat
parseChar :: Char -> Seats
parseChar 'L' = Empty
parseChar '.' = Floor
parseChar '#' = Occupied
parseChar c   = error $ "no parse for " ++ [c]

-- parse each line
parseData :: [String] -> [[Seats]]
parseData = map (map parseChar)

-- convert a list of lists of seats into a Board
toBoard :: [[Seats]] -> Board Seats
toBoard xss = Board (0,0) $ listArray ((0,0), (x-1,y-1)) $ concat xss
  where
    x = length xss
    y = length $ head xss

-- calculate the number of occupied seats
numberOfSeats :: Board Seats -> Int
numberOfSeats = length . filter (==Occupied) . elems . board

-- returns the first repeating number
findFirstRepeating :: [Int] -> Int
findFirstRepeating [x] = x
findFirstRepeating (x:y:xs)
  | x == y    = x
  | otherwise = findFirstRepeating (y:xs)

findSolution :: Int -> ((Board a -> Maybe (Board a)) -> Board Seats -> Seats) -> Board Seats -> Int
findSolution maxSeats sight seats = 
  let nextState = extend (changeState maxSeats . neighbor sight)
      states    = iterate nextState seats
      number    = map numberOfSeats states
  in findFirstRepeating number

main :: IO ()
main = do
  contents <- lines <$> readFile "day11.txt"
  let seats = toBoard $ parseData contents
  print $ findSolution 4 immediate seats 
  print $ findSolution 5 first seats
