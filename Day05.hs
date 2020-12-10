module Day05 where

import Data.List

type Ticket = String
type Seat = Int
type Row = Int
type Aisle = Int

main :: IO ()
main = do
    seats <- sort . fmap parseTicket . lines <$> readFile "Day05_input.txt"
    let highestSeat = last seats
    let seatNumDiff = zipWith (\s s' -> (s, s' - s)) seats (tail seats)
    let Just (missingSeatPre, _) = find ((>1) . snd) seatNumDiff
    putStr "Part one: " >> print highestSeat
    putStr "Part two: " >> print (succ missingSeatPre)

parseTicket :: Ticket -> Seat
parseTicket ticket =
    let (row, aisle) = span (`elem` "FB") ticket
        seats = 2^length aisle
    in seats * parseRow row + parseAisle aisle

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
bspParse input =
    let offset = 2^(length input - 1)
    in head input * offset + bspParse (tail input)
