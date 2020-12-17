module Day11.SeatingSystem (seating) where

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Utils (indexXY)

data Seat = Void | Empty | Occupied deriving (Eq, Show, Ord)

type Pos = (Int, Int)

type Room = M.Map Pos Seat

type Strategy = Pos -> Room -> Int

lookup' = flip M.lookup

until2 :: (a -> a -> Bool) -> (a -> a) -> a -> a
until2 p f a
  | p a a' = a
  | otherwise = until2 p f a'
  where
    a' = f a

directions :: [Pos] 
directions = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

room :: [String] -> Room
room = M.fromList . map (\(p, c) -> (p, chair c)) . indexXY
  where
    chair '.' = Void
    chair 'L' = Empty
    chair '#' = Occupied

incPos :: Pos -> Pos -> Pos
{-# INLINE incPos #-}
incPos (x, y) (x', y') = (x + x', y + y')

arround :: Strategy
arround p r = countOccupied . mapMaybe (lookup' r . incPos p) $ directions

scan :: Pos -> Room -> Pos -> Maybe Seat
scan p r d =
  case lookup' r p' of
    Just Void -> scan p' r d
    seat -> seat
  where
    p' = incPos p d

visible :: Strategy
visible p rm = countOccupied . mapMaybe (scan p rm) $ directions

countOccupied :: [Seat] -> Int
countOccupied = length . filter (== Occupied)

rule :: Int -> Seat -> Int -> Seat
rule _ Empty 0 = Occupied
rule n Occupied c | c >= n = Empty
rule _ s _ = s

-- Conway's Game of Life, with different rule(s)
life :: Int -> Strategy -> Room -> Room
life n f r = M.fromList [(p, s') | (p, s) <- M.assocs r, let s' = rule n s (f p r)]

stable = (==)

part1, part2 :: Room -> Int
part1 = countOccupied . M.elems . until2 stable (life 4 arround)
part2 = countOccupied . M.elems . until2 stable (life 5 visible)

seating :: [String] -> (Int, Int)
seating xs = (a, b)
  where
    r = room xs
    a = part1 r
    b = part2 r
