-- QuickCheck : Unit Testing
--    Randomized Property-Based Testing

module Testing where

import Test.QuickCheck hiding ((===))

-- Verify reverse:

prop_revop_bad :: [Int] -> [Int] -> Bool
prop_revop_bad xs ys = reverse (xs ++ ys) == (reverse xs) ++ (reverse ys)

prop_revop :: [Int] -> [Int] -> Bool
prop_revop xs ys = reverse (xs ++ ys) == (reverse ys) ++ (reverse xs)

