module Day4.PassportProcessing (passportProcessing) where

import Utils (splitWhen, stoi)
import Data.List (isSuffixOf, isPrefixOf, isSubsequenceOf)
import Data.Char (isDigit, isHexDigit)

type Field = (String,String)
type Passport = [ Field ]

isNumber :: String -> Bool
isNumber = all ( isDigit )

inRange :: Int -> Int -> String -> Bool
inRange low high v = isNumber v && n >= low && n <= high 
    where n = stoi v

validEyeColors = [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

-- rules
fieldIsValid :: Field -> Bool
fieldIsValid ("byr", v) = inRange 1920 2002 v
fieldIsValid ("iyr", v) = inRange 2010 2020 v
fieldIsValid ("eyr", v) = inRange 2020 2030 v
fieldIsValid ("ecl",v) = length v == 3 && isSubsequenceOf [v] validEyeColors
fieldIsValid ("pid", v) = isNumber v && length v == 9

fieldIsValid ("hgt", v) 
    | isSuffixOf "in" v = inRange 59 76 leadingDigits
    | isSuffixOf "cm" v = inRange 150 193 leadingDigits
    | otherwise = False
    where leadingDigits = takeWhile isDigit v

fieldIsValid ("hcl", v) = head v == '#' && length p == 6 && all isHexDigit p
    where p = drop 1 v

fieldIsValid (_,_) = True -- ignore others

tpl s = (k,v)
    where k:v:_ = splitWhen (==':') s

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

hasAllMandatory :: Passport -> Bool
-- all required fields MUST be IN passport, the inverse is wrong.
hasAllMandatory xs = all (\fn -> elem fn $ map fst xs) requiredFields    

parse :: [String] -> [Passport]
-- cluster by empty lines -> separate by spaces -> tuples from ':'
parse = map (map tpl . concat . map (splitWhen (==' '))) . splitWhen (=="")

part1 :: [Passport] -> [Passport]
part1 = filter hasAllMandatory

part2 :: [Passport] -> [Passport]
part2 = filter (all fieldIsValid) . part1

passportProcessing :: [String] -> Int
passportProcessing = length . part2 . parse
