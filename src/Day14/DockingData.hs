module Day14.DockingData (docking) where

import Data.Bifunctor (second)
import Data.Bits
import qualified Data.HashMap.Strict as M
import Data.List (foldl', isPrefixOf, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Utils (stoi)

data TriBit = H | L | F deriving (Eq, Show)

data Ins
  = Mask [(Int, TriBit)]
  | Write Integer Integer
  deriving (Eq, Show)

parse :: String -> Ins
parse s
  | "mask" `isPrefixOf` s = Mask . zipWith (curry (second ctoi)) [0 ..] . reverse . (!! 1) . splitOn " = " $ s
  | otherwise = asWrite . map read . splitOn "] =" . fromJust . stripPrefix "mem[" $ s
  where
    asWrite (a : v : _) = Write a v
    ctoi '1' = H
    ctoi '0' = L
    ctoi 'X' = F

writeBit :: (Int, TriBit) -> Integer -> Integer
writeBit (n, L) i = clearBit i n
writeBit (n, H) i = setBit i n
writeBit _ i = i

applyMask :: Ins -> Integer -> Integer
applyMask (Mask m) n = foldr writeBit n m

isMask :: Ins -> Bool
isMask (Mask _) = True
isMask _ = False

decoderV1 :: [Ins] -> [Ins]
decoderV1 [] = []
decoderV1 (Mask m : xs) = v ++ decoderV1 r
  where
    (h, r) = break isMask xs
    v = map (\(Write a v) -> Write a (applyMask (Mask m) v)) h
decoderV1 _ = error "group should start with as mask ins"

expndAdr :: [(Int, TriBit)] -> Integer -> [Integer]
expndAdr [] i = [i]
expndAdr ((n, F) : xs) i = expndAdr xs (clearBit i n) ++ expndAdr xs (setBit i n)
expndAdr ((n, H) : xs) i = expndAdr xs (setBit i n)
expndAdr (_ : xs) i = expndAdr xs i

writes :: [(Int, TriBit)] -> Ins -> [Ins]
writes m (Write a v) = map (`Write` v) $ expndAdr m a

decoderV2 :: [Ins] -> [Ins]
decoderV2 [] = []
decoderV2 (Mask m : xs) = v ++ decoderV2 t
  where
    (h, t) = break isMask xs
    v = concatMap (writes m) h
decoderV2 _ = error "group should start with as mask ins"

collect :: [Ins] -> Integer
collect = sum . M.elems . foldl' (\m (Write a v) -> M.insert a v m) M.empty

part1, part2 :: [Ins] -> Integer
part1 = collect . decoderV1
part2 = collect . decoderV2

docking :: [String] -> (Integer, Integer)
docking xs = (a, b)
  where
    p = map parse xs
    a = part1 p
    b = part2 p
