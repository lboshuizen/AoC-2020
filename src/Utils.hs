module Utils where

import Data.Char (isDigit)
import Data.List (foldr, groupBy, sortBy)
import Data.List.Split (splitWhen)
import Data.Ord (comparing)

-- creates an XY map/chart
-- [".#.","#.."] => [
--                    ((0,0),'.'),((1,0),'#'),((2,0),'.'),
--                    ((0,1),'#'),((1,1),'.'),((2,1),'.')
--                  ]
-- Note: origin/xy:(0,0) is top left
indexXY :: Integral n => [[a]] -> [((n, n), a)]
--indexXY :: [[a]] -> [((x,y),a)]
indexXY xs = concat $ [[((x, y), c) | (x, c) <- zip [0 ..] r] | (y, r) <- zip [0 ..] xs]

indexYX :: Integral n => [[a]] -> [((n, n), a)]
indexYX xs = concat $ [[((y, x), c) | (x, c) <- zip [0 ..] r] | (y, r) <- zip [0 ..] xs]

bool2num :: Int -> Bool -> Int
bool2num n b = if b then n else 0

transpose :: [((Int, Int), a)] -> [((Int, Int), a)]
transpose = map (\((x, y), c) -> ((y, x), c))

dimensions :: [[a]] -> (Int, Int)
dimensions xs = (length . head $ xs, length xs)

-- HACK: fixes leading +/- sign on number...
stoi :: String -> Int
stoi ('-' : s) = - stoi s
stoi ('+' : s) = stoi s
stoi s = read s :: Int

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

unpack :: [Maybe a] -> [a]
unpack xs = [x | Just x <- xs]

split :: Eq a => a -> [a] -> [[a]]
split p = splitWhen (== p)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

combinations :: [a] -> [a] -> [(a, a)]
combinations lx rx = [(l, r) | l <- lx, r <- rx]

partitions :: Int -> [a] -> [[a]]
partitions n [] = []
partitions n xs = h : t
  where
    h = take n xs
    t = partitions n (drop n xs)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

smallest :: (Ord a) => (a, b) -> (a, b) -> (a, b)
smallest (n, s) (n', s')
  | n < n' = (n, s)
  | otherwise = (n', s')

largest :: (Ord a) => (a, b) -> (a, b) -> (a, b)
largest (n, s) (n', s')
  | n > n' = (n, s)
  | otherwise = (n', s')

sortsnd :: (Ord b) => [(a, b)] -> [(a, b)]
sortsnd = sortBy (comparing snd)

sortfst :: (Ord a) => [(a, b)] -> [(a, b)]
sortfst = sortBy (comparing fst)

groupSnd :: (Eq b) => [(a, b)] -> [[(a, b)]]
groupSnd = groupBy (\a b -> snd a == snd b)

combine :: [a] -> [(a, [a])]
combine xs = [(a, xs) | a <- xs]

toDegrees :: Float -> Float
toDegrees rad = rad * 180 / pi

toRadians :: Float -> Float
toRadians deg = deg * pi / 180

tf :: Integral a => a -> Float
tf n = fromIntegral n :: Float

rotations :: [a] -> [[a]]
rotations [] = []
rotations (x : xs) = (xs ++ [x]) : rotations (xs ++ [x])

rotate :: [a] -> [[a]]
rotate xs = take (length xs) (rotations xs)

isNumeric :: String -> Bool
isNumeric = all isDigit
