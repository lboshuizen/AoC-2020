module Day16.TicketTranslation (ticket) where

import           Data.Bifunctor  (second)
import           Data.List       (intersect, isPrefixOf, length, partition,
                                  sortBy, transpose, (\\))
import           Data.List.Split (splitOn)
import           Data.Ord        (comparing)

-- A rule has a name and one or more ranges (start,end)
type Rule = (String, [(Int, Int)])

-- *** parsing ***

intList :: [String] -> [[Int]]
intList = map ( map read . splitOn ",")

ignoreTill :: String -> [String] -> [String]
ignoreTill s = drop 1 . dropWhile (not . isPrefixOf s)

parseTicket :: [String] -> [Int]
parseTicket =  head . intList . take 1 . ignoreTill "your"

parseNearby :: [String] -> [[Int]]
parseNearby =  intList . ignoreTill "nearby"

-- *** solving ***

validRule :: Int -> [(Int, Int)] -> Bool
validRule n = any (\(s, e) -> n >= s && n <= e)

anyValid :: Int -> [Rule] -> Bool
anyValid n = any (validRule n . snd)

invalidFields :: [Rule] -> [Int] -> [Int]
invalidFields rx tx = [ n | n <- tx, not $ anyValid n rx ]

matchField :: [Rule] -> Int -> [String]
matchField rx n = [ fst r | r <- rx, validRule n (snd r)]

matchFields :: [Rule] -> [Int] -> [[String]]
matchFields rx = map (matchField rx)

reduce :: Ord a => [[a]] -> [a]
reduce (x:xs) = foldr intersect' x xs
    where intersect' [] s = s
          intersect' s [] = s
          intersect' s s' = s `intersect` s'

minFields :: [Rule] -> [Int] -> [String]
minFields r = reduce . matchFields r

stable :: Eq a => [(Int,[a])] -> [(Int,[a])]
stable xs
    | all (len 1) xs = xs
    | otherwise = stable (o++diffs)
    where
        (o,r) = partition (len 1) xs
        st = concatMap snd o
        diffs = map (\(n,fl) -> (n,fl \\ st) ) r
        len n l = n == length (snd l)

fields :: [Rule] -> [[Int]] -> [String]
fields r = concatMap snd . sortBy (comparing fst) . stable . zip [0..] . map (minFields r) . transpose

extract :: [Int] -> [String] -> [Int]
extract yt = map fst . filter (\(_,fn) -> "departure" `isPrefixOf` fn) . zip yt

part2 r nb yt = product . extract yt $ fields r nb

part1 = sum . concatMap (invalidFields rules)

ticket xs = (part1 nb, part2 rules nb t)
    where
          nb = parseNearby xs
          t = parseTicket xs

-- lazy, less work to extract the rules instead of parsing our
rules :: [Rule]
rules = [
        ("departure location", [(49,258),(268,960)]),
        ("departure station", [( 37,117),(128,968)]),
        ("departure platform", [( 31,70 ),( 78,974)]),
        ("departure track", [( 26,234 ),( 247,952)]),
        ("departure date", [( 49,625 ),( 635,969)]),
        ("departure time", [( 26,777 ),( 799,974)]),
        ("arrival location", [( 49,735 ),( 757,971)]),
        ("arrival station", [( 28,381 ),( 399,970)]),
        ("arrival platform", [( 49,77 ),( 95,957)]),
        ("arrival track", [( 29,467 ),( 477,950)]),
        ("class", [( 40,218 ),( 234,967)]),
        ("duration", [( 45,900 ),( 911,970)]),
        ("price", [( 42,442 ),( 452,966)]),
        ("route", [( 45,104 ),( 112,953)]),
        ("row", [( 49,877 ),( 884,957)]),
        ("seat", [( 40,168 ),( 184,953)]),
        ("train", [( 43,913 ),( 920,949)]),
        ("type", [( 43,292 ),( 315,955)]),
        ("wagon", [( 48,547 ),( 558,954)]),
        ("zone", [( 40,929 ),( 935,954)])
    ]
