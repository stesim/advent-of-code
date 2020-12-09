module Day01 where

main :: IO ()
main = do
  numbers <- map read . lines <$> readFile "Day01_input.txt"
  let findSummands' = findSummands numbers 2020
  putStrLn $ "Part one: " ++ show (product <$> findSummands' 2)
  putStrLn $ "Part two: " ++ show (product <$> findSummands' 3)

findSummands :: [Int] -> Int -> Int -> Maybe [Int]
findSummands [] _ _ = Nothing
findSummands numbers sum 1 = if sum `elem` numbers then Just [sum] else Nothing
findSummands (numHead:numTail) sum n =
  case findSummands numTail (sum - numHead) (n - 1) of
    Just summands -> Just (numHead:summands)
    Nothing -> findSummands numTail sum n
