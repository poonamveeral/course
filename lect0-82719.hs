{-

[x * 2 | x <- [1,2,3,4]]

l = []
for x in [1,2,3,4] {
  for y in ['a','b','c'] {
     add((x,y),l)
  }
}

name :: Type
name x1 x2 .... xi = body
-}

import Prelude

l1 :: [Int]
l1 = [1,2,3]

p1 :: (Int,[Int])
p1 = (1,l1)




