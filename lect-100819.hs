-- Functional Iffy

-- Syntax
data Expr = T
          | F
          | And Expr Expr
          | Or Expr Expr
          | If Expr Expr Expr -- If condition then else
 deriving (Eq, Show)

ex1 :: Expr
ex1 = And T F

ex2 :: Expr
ex2 = If (Or T T) F T

ex3 :: Expr
ex3 = If (And T F) (Or F T) (If T F T)

-- CBV Evaluator
cbv_eval :: Expr -> Expr
cbv_eval (And b1 b2) =
  case (b1',b2') of
    (T, T) -> T
    (F, T) -> F
    (T, F) -> F
    (F, F) -> F
    (_, _) -> And b1' b2'
 where
   b1' = cbv_eval b1
   b2' = cbv_eval b2
cbv_eval (Or b1 b2) = case (b1',b2') of
    (T, T) -> T
    (T, F) -> T
    (F, T) -> T
    (F, F) -> F
    (_, _) -> Or b1' b2'
 where
   b1' = cbv_eval b1
   b2' = cbv_eval b2
cbv_eval (If b1 b2 b3) =
  case b1' of
    T -> b2'
    F -> b3'
    _ -> If b1' b2' b3'
 where
   b1' = cbv_eval b1
   b2' = cbv_eval b2
   b3' = cbv_eval b3
cbv_eval b = b

-- Lazy Evaluator
lazy_eval :: Expr -> Expr
lazy_eval (And b1 b2) =
  case (b1',b2') of
    (T, T) -> T
    (F, T) -> F
    (T, F) -> F
    (F, F) -> F
    (_, _) -> And b1' b2'
 where
   b1' = lazy_eval b1
   b2' = lazy_eval b2
lazy_eval (Or b1 b2) = case (b1',b2') of
    (T, T) -> T
    (T, F) -> T
    (F, T) -> T
    (F, F) -> F
    (_, _) -> Or b1' b2'
 where
   b1' = lazy_eval b1
   b2' = lazy_eval b2
lazy_eval (If b1 b2 b3) =
  case b1' of
    T -> eval b2
    F -> eval b3
    _ -> If b1' b2 b3
 where
   b1' = lazy_eval b1
   b2' = lazy_eval b2
   b3' = lazy_eval b3
lazy_eval b = b
