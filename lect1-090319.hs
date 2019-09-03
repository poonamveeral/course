import Prelude hiding (odd)

-- Functions:

-- name :: a1 -> a2 -> a3 -> ... -> an -> b
-- name x1 x2 x3 ... xn = body

five :: Integer
five = 5

-- Least divisor greater than or equal to k:
-- 
-- 1. If divides k n, then ldf k n = k
-- 2. If k^2 > n, then ldf k n = n
-- 3. Otherwise ldf k n = ldf (k+1) n

divides :: Integer -> Integer -> Bool
divides k n = (n `rem` k) == 0

ldf1 :: Integer -> Integer -> Integer
ldf1 k n = if k `divides` n
           then k
           else if k^2 >n
                then n
                else ldf1 (k+1) n

{-
  name x1 ... xn | b1 = t1
                 | b2 = t2
                 ...
                 | bm = tm

  name x1 ... xn | b1 = t1
  name x1 ... xn | b2 = t2
  ...
  name x1 ... xn | bm = tm
-}
ldf :: Integer -> Integer -> Integer
ldf k n | k `divides` n = k
        | k^2 > n = n
        | otherwise = ldf (k+1) n

ld :: Integer -> Integer
ld n = ldf 2 n

isPrime :: Integer -> Bool
isPrime n | n < 1 = error "Positive numbers only."
          | n == 1 = False
          | otherwise = ld n == n

-- Pattern Matching:

or1 :: Bool -> Bool -> Bool
or1 b1 b2 | b1 == True = True
          | b2 == True = True
          | otherwise  = False

or2 :: Bool -> Bool -> Bool
or2 True b2 = True
or2 b1 True = True
or2 b1 b2 = False

or3 :: Bool -> Bool -> Bool
or3 True _ = True
or3 _ True = True
or3 _ _    = False

or4 :: Bool -> Bool -> Bool
or4 False False = False
or4 _     _     = True

odd :: Integer -> Bool
odd 0 = False
odd 1 = True
odd 2 = False
odd 3 = True
{-

[1,2,3,4,5,6]

List constructors:
  - Empty List :: []
  - Cons: (:) :: a -> [a] -> [a]
                 |     |      |
                 |     |      |
               head   tail   list with head and tail

    x1 : x2 : x3 : x4 : ... : xn : []

    [1,2,3,4,5,6] == (1 : 2 : 3 : 4 : 5 : 6 : [])

-}

firstInt :: [Int] -> Int
firstInt [] = error "Empty List Not Allowed!"
firstInt (x:rest) = x

ext :: [a] -> (a, [a])
ext [] = error "Beware empty list!"
ext (x:xs) = (x,xs)

double :: [Int] -> [Int]
double [] = []
double (x:xs) = let r = double xs
                 in (2*x):r

double' :: [Int] -> [Int]
double' [] = []
double' (x:xs) = (2*x):r
 where
   r = double' xs

proj1 :: (a,b) -> a
proj1 (x,y) = x
 
proj2 :: (a,b) -> b
proj2 (x,y) = y


