import Data.List

main :: IO ()
solve :: [Int] -> [Int]
solve [x, y] = [min x y, abs (x - y) `div` 2]

printSolved :: (Show a) => [a] -> IO ()
printSolved xs = putStrLn $ unwords (map show xs)

main = do
  text_input <- getLine
  let num_input = map read (words text_input) :: [Int]
  printSolved $ solve num_input
