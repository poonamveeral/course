-- Homework 1:
--   Due: Thursday, Sept. 5 by 12:45pm.
--   Total Points: 20
--   Course: CSCI-3300

-- Before you begin, please ready Question 4.

-- Question 0 (5 pt): Define the logical implication function based on
-- its truth table.  This function takes two arguments.

impL :: Bool -> Bool -> Bool
impL = undefined

-- Question 1 (5 pt): Define the previous function using only notL and
-- orL from the lecture notes.  This function should not case split
-- over the input.  Hint: Compare the truth table of implication and
-- disjunction (orL).

notL :: Bool -> Bool
notL = undefined

orL :: Bool -> Bool -> Bool
orL = undefined

impL' :: Bool -> Bool -> Bool
impL' = undefined

-- Question 2 (5 pt): First, what's the type of the following functions?
-- Second, give an english explanation of the types, and what the
-- functions compute.  This explanation should be something anyone can
-- understand without knowledge of Haskell.

myst' True = "a"
myst' False = "b"

myst b 0 0 = ""
myst b 0 m = (myst' b)++(myst b 0 (m-1))
myst b n 0 = (myst' (not b))++(myst b (n - 1) 0)
myst b n m = (myst' b)++(myst b (n-1) (m-1))++(myst' (not b))

-- Question 3 (5pt): Define the Sudan function:
-- http://en.wikipedia.org/wiki/Sudan_function

sudan :: Integer -> Integer -> Integer -> Integer
sudan = undefined

-- Question 4 (5 pt): Turn this homework in via your Github repo.
--
-- 1. You are not allowed to change the name of this file.
--
-- 2. You must complete your homework within this file.
--
-- 3. After you are finished completeing this assignment,
--    save your work by committing your new changes to
--    your repo.
--
-- 6. Push your changes on the HWK1_SOL branch to Github.
--
-- 8. Your homework is now submitted.

