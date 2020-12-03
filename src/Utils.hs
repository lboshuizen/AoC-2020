module Utils where

import Data.Ord (comparing)
import Data.List (sortBy, groupBy, foldr)

-- creates an XY map/chart
-- [".#.","#.."] => [
--                    ((0,0),'.'),((1,0),'#'),((2,0),'.'),
--                    ((0,1),'#'),((1,1),'.'),((2,1),'.')
--                  ]
-- Note: origin/xy:(0,0) is top left
indexXY :: Integral n => [[a]] -> [((n,n),a)]
--indexXY :: [[a]] -> [((x,y),a)]
indexXY xs = concat $ [ [((x,y),c) | (x,c) <- zip [0..] r]  | (y,r) <- zip [0..] xs ]

indexYX :: Integral n => [[a]] -> [((n,n),a)]
indexYX xs = concat $ [ [((y,x),c) | (x,c) <- zip [0..] r]  | (y,r) <- zip [0..] xs ]

bool2num :: Int -> Bool -> Int
bool2num n b = if b then n else 0

transpose :: [((Int,Int),a)] -> [((Int,Int),a)]
transpose = map (\((x,y),c) -> ((y,x),c))

dimensions :: [[a]] -> (Int,Int)
dimensions xs = (length . head $ xs, length xs)

stoi s = read s :: Int

product :: Num a => [a] -> a
product = foldr (*) 1

sum :: Num a => [a] -> a
sum = foldr (+) 0

readLines :: FilePath -> IO [String]
readLines fname = 
   do
     content <- readFile fname
     let ls = lines content
     return ls

unpack :: [Maybe a] -> [a]
unpack xs = [x | Just x <- xs]

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) 
            where (x,y) = span (/= d) s

reduce :: (a -> a -> a) -> [a] -> a
reduce f (h:t) = foldl f h t

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

countWhere :: (a -> Bool) -> [a] -> Int
countWhere f = length . filter f

combinations :: [a] -> [a] -> [(a,a)]
combinations lx rx = [(l,r) | l <- lx, r <- rx ]

partitions :: Int -> [a] -> [[a]]
partitions n [] = []
partitions n xs = h:t
    where h = take n xs
          t = partitions n (drop n xs)

both :: Eq b => (a -> b) -> a -> a -> Bool
both f a b = f a == f b

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

smallest :: (Ord a) => (a,b) -> (a,b) -> (a,b)
smallest (n,s) (n',s') | n < n' = (n,s)
                       | otherwise = (n',s')

largest :: (Ord a) => (a,b) -> (a,b) -> (a,b)
largest (n,s) (n',s') | n > n' = (n,s)
                       | otherwise = (n',s')

sortsnd :: (Ord b) => [(a,b)] -> [(a,b)]
sortsnd = sortBy (comparing snd)

sortfst :: (Ord a) => [(a,b)] -> [(a,b)]
sortfst = sortBy (comparing fst)

groupSnd :: (Eq b) => [(a,b)] -> [[(a,b)]]
groupSnd = groupBy (\a b -> snd a == snd b)

combine :: [a] -> [(a,[a])]
combine xs = [(a,xs) | a <- xs]

indexed :: [b] -> [(Int, b)]
indexed = zip [0..]

toDegrees :: Float -> Float
toDegrees rad = rad * 180 / pi

toRadians :: Float -> Float
toRadians deg = deg * pi / 180

tf :: Integral a => a -> Float    
tf n = fromIntegral n :: Float

rotations :: [a] -> [[a]]
rotations [] = []
rotations (x:xs) = (xs++[x]):(rotations (xs++[x]) )

rotate :: [a] -> [[a]]
rotate xs = take (length xs) (rotations xs)