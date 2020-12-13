module Day07 where

import Control.Applicative
import Data.Char
import Data.Foldable
import qualified Data.Map as M
import Text.Parsec hiding ((<|>), many, optional)

type Bag = (String, String);
type Requirement = (Bag, Int)
type Rule = (Bag, [Requirement])
type Regulations = M.Map Bag [Requirement]

main :: IO ()
main = do
  (solution1, solution2) <- solve <$> readFile "Day07_input.txt"
  putStr "Part one: " >> print solution1
  putStr "Part two: " >> print solution2

shinyGoldBag :: Bag
shinyGoldBag = ("shiny", "gold")

solve :: String -> (Int, Int)
solve input = (length $ containersOf rules shinyGoldBag, bagsIn rules shinyGoldBag)
  where Right rules = M.fromList <$> parse (many rule) "(error)" input

containersOf :: Regulations -> Bag -> [Bag]
containersOf rules bag = M.keys $ M.filter (== True) finalState
  where finalState = M.foldrWithKey' folder M.empty rules
        folder bag' _ state = snd $ contains rules bag bag' state

type VisitState = M.Map Bag Bool

contains :: Regulations -> Bag -> Bag -> VisitState -> (Bool, VisitState)
contains rules bag container state = case M.lookup container state of
  Just True  -> (True, state)
  Just False -> (False, state)
  Nothing    -> (anyChildIsContainer, M.insert container anyChildIsContainer state')
  where (anyChildIsContainer, state') = foldr' folder (False, state) (rules M.! container)
        folder (bag', _) (result, state)
          | result = (result, state)
          | bag' == bag = (True, state)
          | otherwise = contains rules bag bag' state

bagsIn :: Regulations -> Bag -> Int
bagsIn rules bag = foldr' folder 0 (rules M.! bag)
  where folder (bag', n) prevBags = prevBags + n * (1 + bagsIn rules bag')

rule :: Parsec String s Rule
rule = liftA2 (,) subject requirements <* string ".\n"
  where subject = bagColor <* string " bags contain "
        requirements = [] <$ try (string "no other bags")
                       <|> sepBy1 requirement (string ", ")

requirement :: Parsec String s Requirement
requirement = liftA2 (flip (,)) quantity color <* string " bag" <* optional (char 's')
  where quantity = digitToInt <$> digit
        color = space *> bagColor

bagColor :: Parsec String s Bag
bagColor = liftA2 (,) word (space *> word)

word :: Parsec String s String
word = many1 (oneOf ['a'..'z'])
