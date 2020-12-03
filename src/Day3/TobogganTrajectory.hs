module Day3.TobogganTrajectory (tobogganTrajectory) where

import qualified Data.Set as S
import Utils (indexYX, dimensions)

ignoreEmpty = filter (\(_,c) -> c /= '.' )
justPoints = map (\(p,_) -> p)
asSet = S.fromDistinctAscList

isTree :: Ord a => S.Set a -> a -> Bool
isTree = flip (S.member)

steps (w,h) (sx,sy) = zip yl xl
  where yl = [y | y <- [0,sy..h]]
        xl = [x `mod` w | x <- [0,sx..] ]  

treesOnPath roadMap = length . filter (isTree roadMap)
makeMap = asSet . justPoints . ignoreEmpty . indexYX

tobogganTrajectory :: [String] -> [(Int,Int)] -> Int
tobogganTrajectory xs slopes = product . map (treesOnPath roadmap . steps mapSize) $ slopes 
    where mapSize = dimensions xs
          roadmap = makeMap xs
