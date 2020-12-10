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
  where
    entries = P.filter (not . T.null) $ split isSpace string

parseEntry :: Text -> (Text, Text)
parseEntry string = (name, T.tail valueStr)
  where
    (name, valueStr) = T.break (== ':') string

isPassportValid1 :: Passport -> Bool
isPassportValid1 entries = P.all (`elem` entryNames) requiredFields
  where
    entryNames = fmap fst entries

isPassportValid2 :: Passport -> Bool
isPassportValid2 entries = P.all validate requiredFields
  where
    validate name = let entry = L.find (\(name', _) -> name' == name) entries
                    in (validateEntry <$> entry) == Just True

validateEntry :: PassportEntry -> Bool
validateEntry entry = case entry of
  ("byr", y) -> validateYear 1920 2002 y
  ("iyr", y) -> validateYear 2010 2020 y
  ("eyr", y) -> validateYear 2020 2030 y
  ("hgt", h) -> validateHeight h
  ("hcl", c) -> validateHairColor c
  ("ecl", c) -> validateEyeColor c
  ("pid", i) -> validatePassportID i
  ("cid", i) -> validateCountryID i

validateYear :: Int -> Int -> Text -> Bool
validateYear min max year = year' `isInRange` (min, max)
  where year' = readT year

validHeightUnits :: [Text]
validHeightUnits = ["cm", "in"]

validateHeight :: Text -> Bool
validateHeight height = unitIsValid && magnitude `isInRange` validRange
  where
    (magnitudeText, unit) = T.span isDigit height
    magnitude = readT magnitudeText
    unitIsValid = unit `elem` validHeightUnits
    validRange = case unit of
      "cm" -> (150, 193)
      "in" -> (59, 76)

validateHairColor :: Text -> Bool
validateHairColor color = T.length color == 7 &&
                          T.head color == '#' &&
                          T.all (\c -> isDigit c || c `elem` ['a' .. 'f']) (T.tail color)

validEyeColors :: [Text]
validEyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validateEyeColor :: Text -> Bool
validateEyeColor color = color `elem` validEyeColors

validatePassportID :: Text -> Bool
validatePassportID id = T.length id == 9 && T.all isDigit id

validateCountryID :: Text -> Bool
validateCountryID _ = True

isInRange :: Int -> (Int, Int) -> Bool
isInRange n (min, max) = n >= min && n <= max

readT :: Read a => Text -> a
readT = read . unpack
