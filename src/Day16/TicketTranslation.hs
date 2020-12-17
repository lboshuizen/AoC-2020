module Day16.TicketTranslation (ticket) where

import           Data.Bifunctor  (second)
import           Data.List       (foldr1, intersect, isPrefixOf, length, nub,
                                  partition, sortBy, transpose, (\\))
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

parseRule :: String -> Rule
parseRule s = (lbl,[(a,b),(c,d)])
    where
        (lbl:rng:_) = splitOn ":" s
        (a:b:c:d:_) = map stoi . splitOrDash $ rng
        splitOrDash = concatMap (splitOn "-") . splitOn "or"
        stoi s = read s ::Int

parseRules :: [String] -> [Rule]
parseRules = map parseRule . filter (/="") . takeWhile (not . isPrefixOf "your")

-- *** solving ***

len:: Int -> [a] -> Bool
len n l = n == length l

inRange :: Ord a => a -> (a,a) -> Bool
inRange n (s,e) = n >= s && n <= e

reduce :: Eq a => [[a]] -> [a]
reduce = foldr1 intersect . filter (not . len 0)

validRule :: Ord a => a -> [(a, a)] -> Bool
validRule n = any (inRange n)

possibleFields :: [Rule] -> [Int] -> [String]
possibleFields rx = reduce . map matchField  . nub
    where
        matchField n = [ fst r | r <- rx, validRule n (snd r)]

fit :: Eq a => [(Int,[a])] -> [(Int,[a])]
fit xs
    | all (len 1 . snd) xs = xs
    | otherwise = fit (placed++rest)
    where
        (placed,unstables) = partition (len 1 . snd) xs
        stables = concatMap snd placed
        rest = map (second (\\ stables)) unstables

fields :: [Rule] -> [[Int]] -> [String]
fields r = names . fit . fieldsPerPos
    where
        fieldsPerPos = zip [0..] . map (possibleFields r) . transpose
        names = concatMap snd . sortBy (comparing fst)

extract :: [Int] -> [String] -> [Int]
extract yt = map fst . filter (isPrefixOf "departure" . snd) . zip yt

part2 :: [Rule] -> [[Int]] -> [Int] -> Int
part2 r nb yt = product . extract yt . fields r $ nb

part1 :: [Rule] -> [[Int]] -> Int
part1 r = sum . concatMap invalidFields
    where invalidFields tx = [ n | n <- tx, not . any (validRule n . snd) $ r ]

-- *** Solution

ticket :: [String] -> (Int,Int)
ticket xs = (part1 r nb, part2 r nb t)
    where
          nb = parseNearby xs
          t = parseTicket xs
          r = parseRules xs
