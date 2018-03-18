

data JumpState = JumpState { moves :: [Int], cursor :: Int }


main :: IO ()
main = do
  input <- readFile "day05.txt"
  let jumps = map read $ lines input :: [Int]
  print $ show $ length jumps
