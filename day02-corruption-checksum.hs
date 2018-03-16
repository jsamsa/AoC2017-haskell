import Data.List

--- Day 2: Corruption Checksum ---
-- http://adventofcode.com/2017/day/2

type Sheet = [[Int]]

parse :: String -> Sheet
parse input =
  (map . map) read (map words (lines input)) :: Sheet

checksum :: Sheet -> Int
checksum sheet = let
  mins = map minimum sheet
  maxs = map maximum sheet
  checksums = zipWith (-) maxs mins
  in sum checksums

--testsheet = [[5,9,2,8],[9,4,7,3],[3,8,6,5]] :: [[Int]]

checksum' :: Sheet -> Int
checksum' sheet = let
  divexprs = (map . map) dividePair (map (pairs . reverse . sort)  sheet)
  in sum $ map quotient $ concat $ (map.filter) ((==) 0 . remainder) divexprs

  
pairs :: [a] -> [(a,a)]
pairs as = [(x,y) | (x:ys) <- tails as, y <- ys]

data DivExpr = DivExpr {
    numerator :: Int
  , denominator :: Int
  , quotient :: Int
  , remainder :: Int
  } deriving (Show)

divide :: Int -> Int -> DivExpr
divide num den = let (quot,rem) = divMod num den in DivExpr num den quot rem

dividePair :: (Int, Int) -> DivExpr
dividePair (a,b) = divide a b

main :: IO ()
main = do
  input <- readFile "day02.txt"
  let 
      sheet = parse input
      check1 = checksum sheet
      check2 = checksum' sheet
    in print $ show check1 ++ " and " ++ show check2

