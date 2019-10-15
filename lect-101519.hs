import Prelude hiding (lookup, Eq, (==), Num, (/=))

-- Typeclasses

newtype Ints = Ints Int

class Eq a where
  (==),(/=) :: a -> a -> Bool

  x /= y = not (x == y)

intEq :: Int -> Int -> Bool
intEq 0 0 = True
intEq n1 n2 | n1 >= 0 && n2 >= 0 = (n1 - 1) `intEq` (n2 - 1)
            | n1 < 0  && n2 < 0  = (n1 + 1) `intEq` (n2 + 1)
            | otherwise = False

instance Eq Int where
  n1 == n2 = intEq n1 n2

instance Eq Ints where
  (Ints n1) == (Ints n2) = n1 == n2

lookup :: Eq a => [(a,b)] -> a -> Maybe b
lookup [] _ = Nothing
lookup ((x,y):ps) k | x == k = Just y
                    | otherwise = lookup ps k

-- class Show a where
--   show :: a -> String

-- instance Show Ints where
--   show (Ints n) = "Ints "++show n

instance Show Ints where
  show (Ints n) = show n

data Tree a = Leaf a | Branch a (Tree a) (Tree a)

eqTree :: Eq a => Tree a -> Tree a -> Bool
eqTree (Leaf d1) (Leaf d2) = d1 == d2
eqTree (Branch d1 lt1 rt1) (Branch d2 lt2 rt2) =
  (d1 == d2) && (lt1 `eqTree` lt2) && (rt1 `eqTree` rt2)
eqTree _ _ = False

instance Eq a => Eq (Tree a) where
  (==) = eqTree

showT :: Show a => Tree a -> String
showT (Leaf d) = show d
showT (Branch d lt rt) =
  "  "++(show d) ++ "\n" ++ "/" ++ "   " ++ "\\" ++ "\n" ++ (showT lt) ++ "   " ++ (showT rt)

instance Show a => Show (Tree a) where
  show = showT

-- QuickCheck : Unit Testing
--    Randomized Property-Based Testing

module Testing where

import Data.List
-- import Test.QuickCheck hiding ((===))
