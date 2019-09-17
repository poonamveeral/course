import Prelude hiding (zip, zipWith, foldl, foldr, map, reverse, Nothing, Just, lookup, Maybe)

-- Higher-Order Functions

zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- c zipWith(Fun<a,Fun<b,c>>,List a, List b)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f _ [] = []
zipWith f [] _ = []
zipWith f (x:xs) (y:ys) = (f x y) : zipWith f xs ys

zip' :: [a] -> [b] -> [(a,b)]
zip' l1 l2 = zipWith pair l1 l2
 where
   pair :: a -> b -> (a,b)
   pair x y = (x,y)

zip'' :: [a] -> [b] -> [(a,b)]
zip'' l1 l2 = zipWith (,) l1 l2   

revBad :: [a] -> [a]
revBad [] = []
revBad (x:xs) = (revBad xs) ++ [x]

-- The accumulator pattern:

reverse' :: [a] -> ([a] -> [a])
reverse' acc [] = acc
reverse' acc (x:xs) = reverse' (x:acc) xs

reverse :: [a] -> [a]
reverse l = reverse' [] l

reverse'' :: [a] -> ([a] -> [a])
reverse'' acc [] = acc
reverse'' acc (x:xs) = reverse' (f acc x) xs
 where
   f :: [a] -> a -> [a]
   f acc x = x:acc

foldl :: (accTy -> inputTy -> accTy) -> accTy -> [inputTy] -> accTy
foldl updateAcc acc [] = acc
foldl updateAcc acc (x:xs) = foldl updateAcc (updateAcc acc x) xs

reverseF :: [a] -> [a]
reverseF l = foldl updateAcc [] l
 where
   updateAcc :: [a] -> a -> [a]
   updateAcc acc x = x:acc

{-
   reverseF [1,2,3]
~> foldl updateAcc [] [1,2,3]
~> foldl updateAcc (updateAcc [] 1) [2,3]
~> foldl updateAcc (updateAcc (updateAcc [] 1) 2) [3]
~> foldl updateAcc (updateAcc (updateAcc (updateAcc [] 1) 2) 3) []
~> (updateAcc (updateAcc (updateAcc [] 1) 2) 3)
~> 3 : (updateAcc (updateAcc [] 1) 2)
~> 3 : (2 : (updateAcc [] 1) )
~> 3 : (2 : (1 : []))

   foldl f acc [x1,x2,x3,...,xn]
~> f (f (...(f (f (f [] x1) x2 x3)...) xn-1) xn
-}

-- Makes type `a` partial.
data Maybe a = Nothing | Just a

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup x [] = Nothing
lookup x ((a,b):ps) | x == a = Just b
                    | otherwise = lookup x ps
