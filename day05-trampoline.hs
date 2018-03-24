module Main where

import qualified Data.Sequence as S

type Cursor = Int
type Count = Int


-- startState = (0, 0, [0,3,0,1,-3])

inside :: Int -> S.Seq Int -> Bool
inside idx xs = idx >= 0 && idx < S.length xs


mutate :: Int -> S.Seq Int -> S.Seq Int
mutate cursor xs = S.adjust inc cursor xs

mutate' :: Int -> S.Seq Int -> S.Seq Int
mutate' cursor xs = S.adjust (if (S.index xs cursor) > 2 then dec else inc) cursor xs

dec :: Int -> Int
dec n = n - 1

inc :: Int -> Int
inc n = n + 1


jump :: Count -> Cursor -> S.Seq Int -> Count
jump count cursor xs
  | inside cursor xs = count `seq` cursor `seq` jump (inc count)
                                                     (cursor + (S.index xs cursor))
                                                     (mutate' cursor xs)
  | otherwise        = count

main :: IO ()
main = do
  strings <- words <$> readFile  "../AoC2017/day05.txt"
  --let strings = words "0 3 0 1 -3"
  print $ jump 0 0 (S.fromList (map read strings :: [Int]))
  
