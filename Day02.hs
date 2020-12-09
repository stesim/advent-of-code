module Day02 where

type Policy = (Int, Int, Char)
type Password = String

main :: IO ()
main = do
  entries <- map parseEntry . lines <$> readFile "Day02_input.txt"
  let numValidPasswords validator = length $ filter (uncurry validator) entries
  putStrLn $ "Part one: " ++ show (numValidPasswords matchesPolicy1)
  putStrLn $ "Part two: " ++ show (numValidPasswords matchesPolicy2)

matchesPolicy1 :: Policy -> Password -> Bool
matchesPolicy1 (minNum, maxNum, char) password = occurrences >= minNum && occurrences <= maxNum
  where
    occurrences = length $ filter (== char) password

matchesPolicy2 :: Policy -> Password -> Bool
matchesPolicy2 (pos1, pos2, char) password = charIsAtPos pos1 /= charIsAtPos pos2
  where
    charIsAtPos n = password !! (n - 1) == char

entryDelimiters :: [Char]
entryDelimiters = "- :"

parseEntry :: String -> (Policy, Password)
parseEntry line = (policy, password)
  where
    [minNumStr, maxNumStr, reqCharStr, password] = splitBy (`elem` entryDelimiters) line
    policy = (read minNumStr, read maxNumStr, head reqCharStr)

splitBy :: (Char -> Bool) -> String -> [String]
splitBy _ [] = []
splitBy pred string = beginning : splitBy pred rest
  where
    (beginning, potentialRest) = break pred string
    rest = dropWhile pred potentialRest
