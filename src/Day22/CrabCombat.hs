module Day22.CrabCombat (combat) where

import qualified Data.HashSet    as S
import           Data.List       (foldl')
import           Data.List.Split (splitWhen)

type GHist = (S.HashSet [Int], S.HashSet [Int])
type Strategy = ([Int] -> [Int] -> Bool)
type Card = Int
type Player = [Card]

stoi s = read s :: Int

update :: GHist -> (Player,Player) -> (Bool,GHist)
update h@(s1,s2) (p1,p2)
    | S.member p1 s1 || S.member p2 s2 = (False, h)
    | otherwise = (True,(S.insert p1 s1,S.insert p2 s2))

newHist :: GHist
newHist = (S.empty,S.empty)

parse :: [String] -> (Player,Player)
parse xs = (crds p1,crds p2)
    where (p1:p2:_) = splitWhen (=="") xs
          crds = map stoi . drop 1

round',sround :: Strategy -> GHist -> (Player,Player) -> (Int,[Card])
round' _ _  (d,[])  = (1,d)
round' _ _  ([],d)  = (2,d)
round' st h deck@(p1,_)
    | cont = sround st h' deck
    | otherwise = (1,p1)
    where (cont,h') = update h deck

sround st h (p1@(c1:d1),p2@(c2:d2))
    | st p1 p2 =   let w = fst $ round' st newHist (take c1 d1,take c2 d2)
                   in case w of
                    1 -> win1
                    2 -> win2
    | c1 > c2 = win1
    | otherwise = win2
    where
        win1 = round' st h (d1 ++ [c1,c2],d2)
        win2 = round' st h (d1,d2 ++ [c2,c1])

ruleSub, noSub :: Strategy
ruleSub p1 p2 = hasCards p1 && hasCards p2
    where hasCards (n:xs) = length xs >= n
noSub _ _ = False

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

part1 deck = score . snd $ round' noSub newHist deck
part2 deck = score . snd $ round' ruleSub newHist deck

combat xs = (part1 deck,part2 deck)
    where deck = parse xs
