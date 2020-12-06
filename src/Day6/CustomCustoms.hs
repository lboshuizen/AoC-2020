module Day6.CustomCustoms (customs) where

--import Utils (splitWhen)
import Data.List (nub, intersect )
import Data.List.Split (splitWhen)

intersections (x:xs) = foldr intersect x xs

part1 = sum . map ( length . nub . concat ) . splitWhen (=="")
part2 = sum . map ( length . intersections ) . splitWhen (=="")

customs  = part2