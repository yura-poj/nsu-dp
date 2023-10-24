
module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where

import Data.List

-- 1
unique :: Eq a => [a] -> Bool
unique [] = True
unique l | (head l) `elem` (tail l) = False
          | otherwise = unique $ tail l

-- 2
pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

-- 3
dels :: Integral a => a -> [a]
dels n = [x | x <- [2..n], n `mod` x == 0]

primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 && odd (b - a) && (intersect (dels a) (dels b)) == []]

-- 4
perfect x = sum[ a | a <- [1..x-1], x `mod` a == 0] == x
 
perfectNumbers :: Integral a => [a]
perfectNumbers = [x | x <- [1..], perfect x]
-- 5
cantorPair :: Integral a=> a -> (a, a)

cantorPair i = (x,y)
  where
      t = floor ((-1 + sqrt (fromIntegral (1 + 8 * i))) / 2)
      y = i - t * (t + 1) `div` 2
      x = t - y

cantorPairs :: Integral a => [(a, a)]
cantorPairs = [cantorPair x | x <- [0..]]

-- 6
minimalDistance :: (Floating a, Ord a) => [(a, a)] -> a
minimalDistance [] = 1 / 0

minimalDistance l | ta == [] = 1 / 0
              | otherwise = min (minimum [sqrt((f-a) * (f-a) + (s-b) * (s-b)) | (a,b) <- ta]) (minimalDistance ta)
              where
                (f,s) = head l
                ta = tail l

