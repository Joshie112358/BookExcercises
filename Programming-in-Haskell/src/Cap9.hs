module Cap9 where

data Op = Add | Sub | Mul | Div | Exp
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y/=1 && x <= y
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
valid Exp x y = y > 0 && y /= 1 && x/= 1
{-
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = True
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0
-}
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys): map(y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- e :: Expr
-- let e = App Mul (App Add (Val 1) (Val 50) ) (App Sub (Val 25) (Val 10) )
-- solution e [1,3,7,10,25,50] 765

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

------------------------------------------------------------
main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 831)

------------------------------------------------------------
type Result = (Expr,Int)


results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) |n > 0]
results ns = [res | (ls,rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]


combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]



-------------------- Ejercicios

--Ejercicio 1
choices' :: [a] -> [[a]]
choices' xs = [ns | y <- subs xs, ns <- perms y  ]
--choices' xs = concat [[ns | ns <- perms y] | y <- subs xs] 


-- Ejercicio 2
remove1 :: Eq a => a -> [a] -> [a]
remove1 x [] = []
remove1 x (y:ys) = if x == y then ys  else y: (remove1 x ys)  


isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] y = True
isChoice (x:xs) y = if elem x y then isChoice xs (remove1 x y) else False
-- x < y

-- Ejercicio 3
{-
It would break the function, because it would try to make oparations with nothing and a number, which would case an error, because it would evaluate "eval", then "valid" and then it would cause an error because it wouldnt be defined.
-}



-- Ejercicio 4

-- El siguiente comando da el numero de expresiones posibles
-- length (concat (map exprs (choices' [1,3,7,10,25,50])))
-- 33,665,406

--El siguiente comando da el numero de expresiones validas
-- length (filter (/= []) (map eval (concat (map exprs (choices' [1,3,7,10,25,50]))))  )
-- Usando el "valid" mejorado: 245,644
-- Usando el "valid" viejo: 4,672,540




-- Ejercicio 5
-- Usando el mismo comando: length (filter (/= []) (map eval (concat (map exprs (choices' [1,3,7,10,25,50]))))  )
-- Pero cambiando la def de "valid" viejo para que: valid Sub _ _ = True y evitando la division entre 0
-- 10,839,369



-- Ejercicio 6
--a) 2,723,321 es el numero de expresiones validas del ejemplo del ejer 5, listo
-- (50*(25+((3-1)^7)))/10 es un ejemplo de una solucion con exponencial

--b)
isInRange :: [Int] -> Int -> Bool
isInRange [] _ = False
isInRange [x] n = abs(x-n) <= d
  where
     d = if n >100 then n `div`100 else 5

solutions2 :: [Int] -> Int -> [Expr]
solutions2 ns n = if not (null s1) then s1 else [e | ns' <- choices ns, e <- exprs ns', isInRange (eval e) n]
  where
    s1 = solutions' ns n
   
--take 100 (solutions2 [1,33,7,10,25,50] 831)


--c)
-- metrica: length (values x)
append :: Expr -> [Expr] -> [Expr]
append x [] = [x]
append x (y:ys) = if length ( values x) <= length (values y) then [x] ++ (y:ys) else [y] ++ (append x ys)

-- append2 :: Int -> [Int] -> [Int]
-- append2 x [] = [x]
-- append2 x (y:ys) = if x <= y then [x] ++ (y:ys) else [y] ++ append2 x ys

ordSol :: [Expr] -> [Expr] -> [Expr]
ordSol [] ys = ys
ordSol (x:xs) [] = ordSol xs [x]
ordSol (x:xs) ys = ordSol xs (append x ys)

--ordSol (take 100 (solutions2 [1,33,7,10,25,50] 831)) []
 
