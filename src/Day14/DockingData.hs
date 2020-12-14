module Day14.DockingData (docking) where

import Data.Bifunctor (second)
import Data.Bits (clearBit, setBit)
import qualified Data.IntMap.Strict as M
import Data.List (foldl', isPrefixOf, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data TriBit = H | L | F deriving (Eq, Show)

type Mask = [(Int, TriBit)]

type Strategy = Mask -> [Ins] -> [Ins]

data Ins
  = MaskIns [(Int, TriBit)]
  | Write Int Int
  deriving (Eq, Show)

parse :: String -> Ins
parse s
  | "mask" `isPrefixOf` s = MaskIns . zipWith (curry (second bit)) [0 ..] . reverse . (!! 1) . splitOn " = " $ s
  | otherwise = asWrite . map read . splitOn "] =" . fromJust . stripPrefix "mem[" $ s
  where
    asWrite (a : v : _) = Write a v
    bit '1' = H
    bit '0' = L
    bit 'X' = F

writeBit :: (Int, TriBit) -> Int -> Int
writeBit (n, L) i = clearBit i n
writeBit (n, H) i = setBit i n
writeBit _ i = i

applyMask :: Mask -> Int -> Int
applyMask m n = foldr writeBit n m

isMask :: Ins -> Bool
isMask (MaskIns _) = True
isMask _ = False

expndAdr :: Mask -> Int -> [Int]
expndAdr [] i = [i]
expndAdr ((n, F) : xs) i = expndAdr xs (clearBit i n) ++ expndAdr xs (setBit i n)
expndAdr ((n, H) : xs) i = expndAdr xs (setBit i n)
expndAdr (_ : xs) i = expndAdr xs i

v1, v2 :: Strategy
v1 m = map (\(Write a v) -> Write a (applyMask m v))
v2 m = concatMap (writes m)
  where
    writes m (Write a v) = map (`Write` v) $ expndAdr m a

decoder :: Strategy -> [Ins] -> [Ins]
decoder _ [] = []
decoder f (MaskIns m : xs) = v ++ decoder f r
  where
    (h, r) = break isMask xs
    v = f m h
decoder _ _ = error "group should start with as mask ins"

collect :: [Ins] -> Int
collect = sumMap . toMap
  where
    toMap = M.fromList . map (\(Write a v) -> (a, v))
    sumMap = M.foldr' (+) 0

part1, part2 :: [Ins] -> Int
part1 = collect . decoder v1
part2 = collect . decoder v2

docking :: [String] -> (Int, Int)
docking xs = (part1 p, part2 p)
  where
    p = map parse xs
