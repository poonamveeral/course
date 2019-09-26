import Prelude hiding (Bool, False, True)

-- Data Types : Defining our own types!
{-

  data Name x1 ... xn = C1 y1 ... ym | ... | Cj z1 ... zl

-}

data Color = Red
           | Blue
           | Green
           | Black
           | Plum
 deriving Show

data Point = MakePoint Int Int
 deriving Show

-- data Maybe a = Nothing | Just a

data List a = Empty | Cons a (List a)

type RGB = (Integer, Integer, Integer)

toRGB :: Color -> RGB
toRGB Red   = (255, 0,   0)
toRGB Blue  = (0,   0,   255)
toRGB Green = (0,   255, 0)
toRGB Black = (0,   0,   0)
toRGB Plum  = (230, 115, 25)

-- data BTree1 nodeType =
--     EmptyT         -- Empty tree
--   | Leaf nodeType  -- Leaf with data of type a
--     -- Branching node with data of type a
--   | Branch nodeType (BTree1 nodeType) (BTree1 nodeType)

data BTree nodeType =
    LBranch nodeType nodeType nodeType
  | Branch nodeType (BTree nodeType) (BTree nodeType)

data Bool = True | False
data X = Y | Z

toX :: Bool -> X
toX True = Y
toX False = Z

fromX :: X -> Bool
fromX Y = True
fromX Z = False

bar :: X -> X -> X
bar Y _ = Y
bar Z _ = Y
bar _ _ = Z 

data Z a = Foo | Bar a
                    
-- S ::= aSb | empty
data A = Mka    -- Think of Mka as just 'a'
 deriving Show
data B = Mkb    -- Think of Mkb as just 'b'
 deriving Show
data S = Step A S B | Empt
 deriving Show

toString :: S -> String
toString Empt = ""
toString (Step Mka w Mkb) = "a" ++ (toString w) ++ "b"
