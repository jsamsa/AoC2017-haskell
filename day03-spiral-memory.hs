import Data.List
import qualified Data.Map.Strict as Map

-- Spiral Memory  Day 3
-- http://adventofcode.com/2017/day/3

location = 289326 :: Int

data Direction = South | East | North | West deriving (Enum, Show)

type Coord = (Int, Int)

data Plane = Plane [Coord]

leftTurn ::  Coord -> Direction -> Plane -> Maybe Coord
leftTurn = undefined

next :: Coord -> Direction -> Coord
next (x,y) North = (x, y + 1)
next (x,y) South = (x, y - 1)
next (x,y) East  = (x + 1, y)
next (x,y) West  = (x - 1, y)


-- 17  16  15  14  13
-- 18   5   4   3  12
-- 19   6   1   2  11
-- 20   7   8   9  10
-- 21  22  23---> ...

-- E N WW SS EEE NNN WWWW SSSS EEEEE NNNNN WWWWWW SSSSSS
-- 1 1 2  2  3   3   4    4    5     5     6      6  

spiral :: [(Int, Direction)]
spiral = concat . transpose $ [zip [1..] (cycle [East, West]), zip [1..] (cycle [North, South])]

expand :: [(Int, Direction)] -> [Direction]
expand = concat . map (\x -> replicate (fst x) (snd x))

generateSpiral =  expand $ spiral         

type Values = Map.Map Coord Int

-- todo research space saving approach (we only need the previous coord in memory)
mkPlane :: Int -> Coord -> [Coord]
mkPlane size start =
  let
    dirs = generateSpiral
  in start : go (size-1) start dirs
  where go n c (x:xs)
          | n > 0     = let c' = next c x in c' : go (n - 1) c' xs
          | otherwise = []

biggerThan :: Int -> Int
biggerThan until =
  let
    start = (0,0)
    path  = generateSpiral
    vals = Map.singleton start 1
    val  = vals Map.! start
  in go start val until vals path
    where go coord value until values path
            | value > until = value
            | otherwise     =
              let
                coord'   = (next coord (head path))
                path'    = tail path
                values'  = neighborValue coord' values
              in go coord' (values' Map.! coord') until values' path'


neighbors :: Coord -> [Coord]
neighbors center = tail $ mkPlane 9 center

neighborValue :: Coord -> Values -> Values
neighborValue coord values =
  let
    s = sum (map (\x -> Map.findWithDefault 0 x values) (neighbors coord))
  in Map.insert coord s values


solvePart1 = let
  start = (0,0)
  (x,y) = last $ mkPlane location start
  in (abs x) + (abs y)

solvePart2 = biggerThan location
