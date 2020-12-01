module Lib.Polar where

import Lib.Types (Point)
import Utils

type Phi = (Int,Int)
type Polar = (Int,Phi)

polar :: (Point) -> Polar
polar (x,y) = (l,phi)
    where f = gcd x y
          l = x^2 + y^2
          phi = (x `div` f,y `div` f)

degrees :: (Phi) -> Float
degrees = toDegrees . radians

radians :: (Phi) -> Float
radians (x,y)
   | x == 0, y < 0 = 3/2 * pi
   | x == 0 = pi/2
   | x < 0 = atn + pi
   | y < 0 = atn + 2 * pi
   | otherwise = atn
   where atn = atan $ (tf y) / (tf x)
