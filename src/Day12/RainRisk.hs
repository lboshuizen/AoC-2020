module Day12.RainRisk (rain) where

import Data.List (foldl')

type Pos = (Int, Int)

type Heading = Int

type ShipH = (Pos, Heading)

type ShipWp = (Pos, Pos)

type Ins = (Char, Int)

parse :: String -> Ins
parse s = (head c, read n :: Int) where (c, n) = splitAt 1 s

project :: Pos -> Int -> Int -> Pos
project (x, y) 0 d = (x + d, y)
project (x, y) 90 d = (x, y - d)
project (x, y) 180 d = (x - d, y)
project (x, y) 270 d = (x, y + d)

moveH :: ShipH -> Ins -> ShipH
moveH ((x, y), h) ('N', v) = ((x, y + v), h)
moveH ((x, y), h) ('S', v) = ((x, y - v), h)
moveH ((x, y), h) ('E', v) = ((x + v, y), h)
moveH ((x, y), h) ('W', v) = ((x - v, y), h)
moveH (p, h) ('L', v) = (p, (h + 360 - v) `mod` 360)
moveH (p, h) ('R', v) = (p, (h + v) `mod` 360)
moveH (p, h) ('F', v) = (project p h v, h)

rotate :: Int -> Pos -> Pos
rotate 0 p = p
rotate 90 (x, y) = (y, negate x)
rotate 180 (x, y) = (negate x, negate y)
rotate 270 (x, y) = (negate y, x)

moveWp :: ShipWp -> Ins -> ShipWp
moveWp (p, (x, y)) ('N', v) = (p, (x, y + v))
moveWp (p, (x, y)) ('S', v) = (p, (x, y - v))
moveWp (p, (x, y)) ('E', v) = (p, (x + v, y))
moveWp (p, (x, y)) ('W', v) = (p, (x - v, y))
moveWp (p, wp) ('R', v) = (p, rotate v wp)
moveWp (p, wp) ('L', v) = (p, rotate (360 - v) wp)
moveWp ((x, y), (x', y')) ('F', v) = ((x + v * x', y + v * y'), (x', y'))

part1 :: [Ins] -> ShipH
part1 = foldl' moveH ((0, 0), 0)

part2 :: [Ins] -> ShipWp
part2 = foldl' moveWp ((0, 0), (10, 1))

rain xs = (a, b)
  where
    ins = map parse xs
    a = manhattan . fst . part1 $ ins
    b = manhattan . fst . part2 $ ins
    manhattan (x, y) = abs x + abs y