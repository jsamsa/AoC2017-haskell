import Data.Monoid
import Data.Char
import System.IO

-- inverse captcha
-- http://adventofcode.com/2017/day/1

tupleEq :: Eq a => (a, a) -> Bool
tupleEq (a, b) = a == b

type PairFun = [Char] -> [(Char, Char)]

pairNext :: PairFun
pairNext input = zip input (tail input ++ [head input]) -- 1234 -> [(1,2), (2,3), ...]

pairHalf :: PairFun
pairHalf input =
  let
    halves = splitAt (div (length input) 2) input
    halved = (snd halves) ++ (fst halves)
  in zip input halved

compute :: String -> PairFun -> Int
compute input pairFun =
  let
    pairs  = pairFun input
    eqs    = filter tupleEq pairs
    msum   = foldMap (Sum . digitToInt . fst) eqs
  in getSum msum

main :: IO ()
main = do
  input <- withFile "day01.txt" ReadMode hGetLine
  print $ "next: " ++ show (compute input pairNext)
  print $ "half: " ++ show (compute input pairHalf)
