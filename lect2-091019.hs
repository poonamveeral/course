import Prelude hiding (map,zip)

-- Higher-Order Functions
--   - Must have if you are to be
--     able to program functionally.
--
--   - Any function that takes a function
--     as input, or returns a function as
--     output.
--
--   - Function types have the following form:
--       a1 -> (a2 -> (a3 -> ... -> (an-2 -> (an-1 -> (an -> b)))))
--
--     Example: Int -> (Int -> Bool)
foo1 :: Int -> (Int -> Bool)
foo1 x y = x <= y

-- Using a lambda-expression, \lambda x.b written as \x -> b where x is th input variable, and b is the body.
foo2 :: Int -> (Int -> Bool)
foo2 x = \y -> x <= y

foo3 :: Int -> (Int -> Bool)
foo3 = \x -> (\y -> x <= y)

{- Case expression:
     case e of
       p1 -> b1
       p2 -> b2
       ...
       pn -> bn
-}
impL :: Bool -> (Bool -> Bool)
impL = \b1 -> \b2 -> case b1 of
                       True -> b2
                       False -> True

-- Taking functions as inputs:
{-
 The map combinator:

 map :: (a -> b) -> ([a] -> [b])
        |             |      |
        |             |     body
        |             l
        f
-}
        
map :: (a -> b) -> ([a] -> [b])
map f [] = []
map f (x:xs) = (f x) : map f xs

type DBEL = (String   -- First Name
           , String   -- Last Name
           , Integer  -- Age
           , Integer  -- ID
           , String)  -- Department

type DBTy = [DBEL]  
createDB :: DBTy  
createDB = [("Mike", "Dowell", 500, (-1), "Computer Science"),
            ("Onyeka", "Enzewoya", 38, 1, "Software Engineering"),
            ("Joanne", "Sexton", 50, (-2), "Cyber")]

firstNameIDAge :: DBEL -> (String, Integer, Integer)
firstNameIDAge (fn, ln, age, id, dp) = (fn, id, age)

firstName :: DBEL -> String
firstName (fn, ln, age, id, dp) = fn

mangle :: Char -> Char
mangle 'a' = '*'
mangle 'b' = '_'
mangle 'c' = '4'
mangle 'h' = 'C'
mangle 's' = '%'
mangle 'k' = '9'
mangle 'l' = '@'
mangle 'e' = 'f'

zip :: [a] -> [b] -> [(a,b)]
zip [] l2 = []
zip l1 [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

{-

   zip [0,1] ['a','b']
=  zip (0:[1]) ('a':['b'])
~> (0,'a'):zip [1] ['b']
=  (0,'a'):zip (1:[]) ('b':[])
~> (0,'a'):((1,'b'):zip [] [])
~> (0,'a'):((1,'b'):[])
=  [(0,'a'),(1,'b')]

   zip [0] ['a','b']
=  zip (0:[]) ('a':['b'])
~> (0,'a'):zip [] ['b']
~> (0,'a'):[]
=  [(0,'a')]

-}
