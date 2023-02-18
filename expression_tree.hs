data Expr = Val Int | Add Expr Expr | Mul Expr Expr

size :: Expr -> Int
size (Val _) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

