module Day17.ConwayCubes (boot) where

import           Data.Bifunctor  (bimap, second)
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe      (Maybe, fromJust, mapMaybe)
import           Utils           (indexXY)

data State = Inactive | Active deriving (Eq,Show,Ord)
type Pos3 = (Int,Int,Int)
type Pos4 = (Int,Int,Int,Int)

type Projection d = (d->d->d, [d])

type Grid d = M.Map d State

d3 (x,y) = (x,y,0)
d4 (x,y) = (x,y,0,0)

parse fd = map (bimap fd state) . indexXY
    where state '.' = Inactive
          state '#' = Active

directions3 :: [Pos3]
directions3 = [p | x <- [-1..1], y <- [-1..1], z <- [-1..1], let p = (x,y,z), p /= (0,0,0) ]

directions4 :: [Pos4]
directions4 = [p | x <- [-1..1], y <- [-1..1], z <- [-1..1], w <- [-1..1]  , let p = (x,y,z, w), p /= (0,0,0,0) ]

proj3 :: Pos3 -> Pos3 -> Pos3
proj3 (x,y,z) (x',y',z') = (x+x',y+y',z+z')

proj4 :: Pos4 -> Pos4 -> Pos4
proj4 (x,y,z,w) (x',y',z',w') = (x+x',y+y',z+z', w+w')

countActive = length . filter (==Active)

scan :: (Eq k, Ord k) => Grid k -> k -> Projection k -> Int
scan g p (proj,dir) = countActive . mapMaybe ml $ dir
    where ml = flip M.lookup g . proj p

grid dim = M.fromList . parse dim

times n f s = snd $ until (\x -> fst x == 0) (bimap pred f) (n,s)

rule :: (Eq k, Ord k) => (Maybe State,Int,k) -> Grid k -> Grid k
rule (Just Active,n, p) g' | n /=2 && n /= 3 = M.insert p Inactive g'
rule (Just Inactive , 3 , p) g' = M.insert p Active g'
rule (Nothing,3,p) g' = M.insert p Active g'
rule (Nothing,_,p) g' = M.insert p Inactive g'
rule _ g' = g'

-- same as nub but way faster
nub' :: Ord a => [a] -> [a]
nub' = S.toList . S.fromList

area :: (Eq k, Ord k) => Grid k -> Projection k -> [k]
area g (proj,dir) = nub' $ [ proj p d | p <- M.keys g, d <- dir]

cubes :: (Eq k, Ord k) => Grid k -> Projection k -> [(Maybe State,Int,k)]
cubes g proj = map (\p -> (M.lookup p g, scan g p proj, p)) (area g proj)

conway :: (Eq k, Ord k) => Projection k -> Grid k -> Grid k
conway proj g = foldr rule g (cubes g proj)

part1, part2 :: [String] -> Int
part1 = countActive . M.elems . times 6 (conway (proj3,directions3)) . grid d3
part2 = countActive . M.elems . times 6 (conway (proj4,directions4)) . grid d4

boot xs = (a,b)
    where a = part1 xs
          b = part2 xs

test = [".#.",
        "..#",
        "###"
    ]
