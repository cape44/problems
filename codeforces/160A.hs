import Data.List

toNumArray :: String -> [Int]
toNumArray = map read . words

val :: Int -> Double
val x = fromIntegral x / 2

solve :: [Int] -> Double -> [Int]
solve [] _ = []
solve (x : xs) val
  | val < 0 = []
  | otherwise = x : solve xs (val - fromIntegral x)

reverseSort :: (Ord a) => [a] -> [a]
reverseSort = sortBy (flip compare)

main :: IO ()
main = do
  input <- getContents
  let ls = toNumArray $ unlines . take 1 . drop 1 . lines $ input
  let coinVal = val $ sum ls
  let result = solve (reverseSort ls) coinVal
  print $ length result
