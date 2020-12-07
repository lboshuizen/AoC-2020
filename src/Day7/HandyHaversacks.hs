module Day7.HandyHaversacks (haversacks) where

import Data.Char (isDigit)
import Data.List
import Data.List.Split
import Utils (stoi)

type Rule = (String, [(Int,String)]) -- deriving (Show)

strip = concat . filter (not . isPrefixOf "bag") . splitOn " "

subs :: String -> [(Int,String)]
subs s | isPrefixOf "no" s = []

subs s = map (ns . span isDigit . strip) . splitOn "," $ s
       where ns (s1,s2) = (stoi s1, s2)

parse :: String -> Rule
parse s = (strip k,subs l)
    where (k:l:_) = splitOn " contain " s

canHold :: String -> Rule -> Bool       
canHold s = any (\(_,c) -> c == s) . snd

parents :: String -> [Rule] -> [Rule]
parents _ [] = []
parents s xs = ch ++ pr
    where ch = filter (canHold s) xs
          pr = concatMap (\c -> parents (fst c) (xs \\ ch)) ch

-- bottom up search
part1 = length . nub . map fst . parents "shinygold" . map parse

-- top down scan
part2 :: String -> [Rule] -> Int
part2 s alst = case lookup s alst of
                   Nothing -> error "impossible"
                   Just [] -> 0
                   Just bs -> foldr (\c a -> a + contained c) 0 bs
                where contained (n,c) = n + n * part2 c alst

haversacks :: [String] -> Int
haversacks = part2 "shinygold" . map parse
