module Day03 where

data Square = Empty | Tree deriving(Eq, Show)
type Map = [[Square]]
type Slope = (Int, Int)

slopes :: [Slope]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
  map <- parseMap <$> readFile "Day03_input.txt"
  let treesAlongSlope slope = length $ filter (== Tree) $ slidePath map 0 slope
  putStrLn $ "Part one: " ++ show (treesAlongSlope (3, 1))
  putStrLn $ "Part two: " ++ show (product $ fmap treesAlongSlope slopes)

slidePath :: Map -> Int -> Slope -> [Square]
slidePath [] _ _ = []
slidePath map x offset@(dx, dy) = firstSquare : rest
  where
    firstSquare = head map !! x
    rest = slidePath (drop dy map) (x + dx) offset

parseMap :: String -> Map
parseMap = map parseMapRow . lines

parseMapRow :: String -> [Square]
parseMapRow = cycle . map toSquare

toSquare :: Char -> Square
toSquare c = case c of
  '.' -> Empty
  '#' -> Tree
