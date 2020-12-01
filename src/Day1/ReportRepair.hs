module Day1.ReportRepair (reportRepair) where

import qualified Data.Set as S

-- head tail of set
ht :: Ord a => S.Set a -> (a, S.Set a)
ht s = (h, S.delete h s)
    where h = S.elemAt 0 s

pair :: Integral a => a -> (a, S.Set a) -> Maybe (a,a)
pair v (n,s)
    | S.null s = Nothing
    | S.member (v-n) s = Just (n,(v-n))
    | otherwise = pair v (ht s)

tripple :: Integral a => a -> (a, S.Set a) -> (a,a,a)
tripple v (a,s) =
    -- v = a + b + c -> v-a = b + c
    case bc of 
        Nothing -> tripple v (ht s)
        Just (b,c) -> (a,b,c)
    where bc = pair (v-a) (ht s)

reportRepair :: Integral a => a -> [a] -> a
reportRepair t xs = a * b * c
    where (a,b,c) = tripple t (ht (s))
          s = S.fromList xs
    