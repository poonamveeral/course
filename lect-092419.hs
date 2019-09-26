{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
import Prelude hiding (foldr, sum, (.), curry, uncurry)

-- Foldr : Encapsulates basic recursion

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f s [] = s
foldr f s (x:xs) = f x (foldr f s xs)

{-
   foldr f s [x1,x2,x3,...,xn-1,xn]
~> f x1 (foldr f s [x2,x3,...,xn-1,xn])
~> f x1 (f x2 (foldr f s [x3,...,xn-1,xn]))
~> f x1 (f x2 (f x3 (foldr f s [,...,xn-1,xn])))
~> f x1 (f x2 (f x3 (...(f xn-1 (foldr f s [xn])))...))
~> f x1 (f x2 (f x3 (...(f xn-1 (f xn (foldr f s []))))...))
~> f x1 (f x2 (f x3 (...(f xn-1 (f xn s)))...))
-}

flatten :: [[a]] -> [a]
flatten l = foldr (++) [] l

sum :: [Integer] -> Integer
sum l = foldr (+) 0 l

mul :: [Integer] -> Integer
mul l = foldr (*) 1 l

fact :: Integer -> Integer
fact n = mul [1..n]

-- Higher-order functions:

(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g (f x)

(<.>) :: (b -> c) -> (a -> b) -> (a -> c)
(g <.> f) x = g (f x)

collapseDouble :: [[Integer]] -> [Integer]
collapseDouble l =((map (2*)). flatten) l

collapseDoublePF :: [[Integer]] -> [Integer]
collapseDoublePF = double . flatten
 where
   double :: [Integer] -> [Integer]
   double = map (2*)

-- Currying: An isomorphism between unary functions and n-ary
-- functions:
--
-- n-ary functions: (a1,...,an) -> b

curry :: ((a,b) -> c) -> (a -> (b -> c))
curry f x y = f (x,y)

curry2 :: ((a,b) -> c) -> (a -> (b -> c))
curry2 f = \x -> (\y -> f (x,y))

uncurry :: (a -> (b -> c)) -> ((a,b) -> c)
uncurry g (x,y) = (g x) y

iso1 :: Eq c => ((a,b) -> c) -> (a,b) -> Bool
iso1 f (x,y) = ((uncurry.curry) f) (x,y) == f (x,y)

iso1' :: Eq ((a,b) -> c) => ((a,b) -> c) -> Bool
iso1' f = ((uncurry.curry) f) == f

iso2 :: Eq (a -> (b -> c)) => (a -> (b -> c)) -> Bool
iso2 f = ((curry.uncurry) f) == f

aux :: (Integer,Integer) -> Integer
aux (x,y) = x + y

-- Polymorphism:

append :: [a] -> [a] -> [a]
append [] l2 = l2
append l1 [] = l1
append (x:xs) l2 = x : append xs l2

-- Parametric Polymorphic Functions:
f :: a -> a
f x = x

fst :: (a,b) -> a
fst (x,y) = x

snd :: (a,b) -> b
snd (x,y) = y

g :: forall a.a -> a
g x = x


