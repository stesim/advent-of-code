{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import Prelude hiding (readFile, putStr, lines)

import Data.Set
import Data.Text hiding (foldl1, empty, head)
import Data.Text.IO

main :: IO ()
main = do
  (solution1, solution2) <- solve <$> readFile "Day06_input.txt"
  putStr "Part one: " >> print solution1
  putStr "Part two: " >> print solution2

solve :: Text -> (Int, Int)
solve input = (sumBy union, sumBy intersection)
  where groupSizesBy combinator = groupSizeBy combinator <$> parseGroups input
        sumBy combinator = sum (groupSizesBy combinator)

groupSizeBy :: (Set Char -> Set Char -> Set Char) -> [Text] -> Int
groupSizeBy combinator group = size $ foldl1 combinator subsets
  where subsets = fromList . unpack <$> group

parseGroups :: Text -> [[Text]]
parseGroups input = lines <$> splitOn "\n\n" input
