import qualified Data.Set as Set
import Data.List (sort)

-- Day 4: High-Entropy Passphrases 
-- http://adventofcode.com/2017/day/4

checkUnique :: String -> Bool
checkUnique line = let
  ws  = words line
  set = Set.fromList ws
  in Set.size set == length ws

checkAnagram :: String -> Bool
checkAnagram line = let
  ws = words line
  set = Set.fromList (map sort ws) 
  in Set.size set /= length ws

main :: IO ()
main = do
  input <- readFile "day04.txt"
  let
    rawlines = lines input
    filtered = filter  (\x -> checkUnique x && (not (checkAnagram x))) rawlines
    count = length filtered
    in print $ show count
