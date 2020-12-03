module Day2.PasswordPhilosophy (passwordPhilosophy) where

import Data.List
import Utils (stoi,split)

policy1 :: (Int,Int,Char,String) -> Bool
policy1 (p1,p2,c,pwd) = cnt >= p1 && cnt <= p2
    where cnt = length . filter (==c) $ pwd

policy2 :: (Int,Int,Char,String) -> Bool
policy2 (p1,p2,c,pwd) 
    | length pwd < p2 = False
    | otherwise = (at p1) /= (at p2)
        where at n = pwd !! (n-1) == c

decode :: String -> (Int,Int,Char,String)
decode s = (p1,p2,c,pwd)
    where (minmax:cc:pwd:_) = words s
          (p1:p2:_)= map stoi . split '-' $ minmax
          c = head cc

conform p = p . decode

passwordPhilosophy = filter (conform policy2)
