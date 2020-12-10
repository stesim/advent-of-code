module Day05 where

import Data.List

type Ticket = String
type Seat = Int
type Row = Int
type Aisle = Int

main :: IO ()
main = do
  (solution1, solution2) <- solve <$> readFile "Day05_input.txt"
  putStr "Part one: " >> print solution1
  putStr "Part two: " >> print solution2

solve :: String -> (Int, Int)
solve input = (last seats, succ missingSeatPre)
  where seats = sort . fmap parseTicket . lines $ input
        seatNumDiff = zipWith (\s s' -> (s, s' - s)) seats (tail seats)
        Just (missingSeatPre, _) = find ((>1) . snd) seatNumDiff

parseTicket :: Ticket -> Seat
parseTicket ticket = seats * parseRow row + parseAisle aisle
  where (row, aisle) = span (`elem` "FB") ticket
        seats = 2^length aisle

parseRow :: String -> Row
parseRow = bspStringParse 'F' 'B'

parseAisle :: String -> Aisle
parseAisle = bspStringParse 'L' 'R'

bspStringParse :: Char -> Char -> String -> Int
bspStringParse cl cr = bspParse . fmap binarify
  where binarify c
          | c == cl = 0
          | c == cr = 1

bspParse :: [Int] -> Int
bspParse []    = 0
bspParse input = head input * offset + bspParse (tail input)
  where offset = 2^(length input - 1)
