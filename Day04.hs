{-# LANGUAGE OverloadedStrings #-}

module Day04 where

import Prelude as P
import Data.Char
import Data.List as L
import Data.Text as T
import Data.Text.IO as T

type PassportEntry = (Text, Text)
type Passport = [PassportEntry]

main :: IO ()
main = do
  passports <- fmap parsePassport . splitOn "\n\n" <$> T.readFile "Day04_input.txt"
  let numValidPassports validator = P.length $ P.filter validator passports
  P.putStrLn $ "Part one: " ++ show (numValidPassports isPassportValid1)
  P.putStrLn $ "Part two: " ++ show (numValidPassports isPassportValid2)

requiredFields :: [Text]
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

parsePassport :: Text -> Passport
parsePassport string = fmap parseEntry entries
  where entries = P.filter (not . T.null) $ split isSpace string

parseEntry :: Text -> (Text, Text)
parseEntry string = (name, T.tail valueStr)
  where (name, valueStr) = T.break (== ':') string

isPassportValid1 :: Passport -> Bool
isPassportValid1 entries = P.all (`elem` entryNames) requiredFields
  where entryNames = fmap fst entries

isPassportValid2 :: Passport -> Bool
isPassportValid2 entries = P.all validate requiredFields
  where validate name = let entry = L.find (\(name', _) -> name' == name) entries
                        in (validateEntry <$> entry) == Just True

validateEntry :: PassportEntry -> Bool
validateEntry (key, value) = validator value
  where validator = case key of
                      "byr" -> validateIntRange 1920 2002
                      "iyr" -> validateIntRange 2010 2020
                      "eyr" -> validateIntRange 2020 2030
                      "hgt" -> validateHeight
                      "hcl" -> validateHexColor
                      "ecl" -> validateColorString
                      "pid" -> validatePassportID
                      "cid" -> const True
                      _     -> const False

validateIntRange :: Int -> Int -> Text -> Bool
validateIntRange min max value = readT value `isInRange` (min, max)

validateHeight :: Text -> Bool
validateHeight height = (isInRange magnitude <$> range) == Just True
  where (magnitude', unit) = T.span isDigit height
        magnitude = readT magnitude'
        range = case unit of
          "cm" -> Just (150, 193)
          "in" -> Just (59, 76)
          _    -> Nothing

validateHexColor :: Text -> Bool
validateHexColor color = T.length color == 7 &&
                         T.head color == '#' &&
                         T.all (`elem` validChars) (T.tail color)
  where validChars = ['0'..'9'] ++ ['a'..'f']

validateColorString :: Text -> Bool
validateColorString color = color `elem` validColors
  where validColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validatePassportID :: Text -> Bool
validatePassportID id = T.length id == 9 && T.all isDigit id

isInRange :: Int -> (Int, Int) -> Bool
isInRange n (min, max) = n >= min && n <= max

readT :: Read a => Text -> a
readT = read . unpack
