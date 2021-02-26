{-# LANGUAGE InstanceSigs #-}


module Cap8 where

-- Ejercicio 1
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult n (Succ Zero) = n
mult n (Succ m) = n `add` (mult n m)


-- Ejercicio 2
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | x == y = True
                      | x < y = occurs x l
                      | otherwise = occurs x r

occurs2 :: Ord a => a -> Tree a -> Bool
occurs2 x (Leaf y) = x == y
occurs2 x (Node l y r) |d == EQ = True
                       |d == LT = occurs2 x l
                       |d == GT = occurs2 x r
                       where
                         d = compare x y

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- Es mas rapido ya que el valor de d solo se calcula una vez y no tiene que hacer 1-2 calculos como la otra version


-- Ejercicio 3
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)
  deriving Show


t2 :: Tree2 Int
t2 = Node2 (Node2 (Leaf2 1)  (Leaf2 4))  (Node2 (Leaf2 6)  (Leaf2 9))

t3 :: Tree2 Int
t3 = Node2 (Node2 (Node2 (Node2 (Leaf2 0) (Leaf2 1)) (Leaf2 2) )  (Leaf2 4))  (Node2 (Leaf2 6) (Leaf2 9))

numLeaves :: Tree2 a -> Int
numLeaves (Leaf2 y) = 1
numLeaves (Node2 l r) = numLeaves l + numLeaves r


balanced :: Tree2 a -> Bool
balanced (Leaf2 _) = True
balanced (Node2 l r) = if abs(numLeaves l - numLeaves r) > 1 then False else (balanced l) && (balanced r)

-- Ejercicio 4
halve2 :: [a] -> ([a], [a])
halve2 x |(length x) `mod` 2 == 0  = (take ((length x) `div` 2) x, drop ((length x) `div` 2) x )
          |(length x) `mod` 2 == 1  = (take ((length (x) - 1) `div` 2) x, drop ((length (x) - 1) `div` 2) x )

balance :: [a] -> Tree2 a
balance [x] = (Leaf2 x)
balance x = Node2 (balance( fst (halve2 x))) (balance (snd (halve2 x )))


-- Ejercicio 5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

--folde (*2) (+) (Add (Add (Val 2) (Val 3)) (Val 4))
--18
--folde (*2) (*) (Add (Add (Val 2) (Val 3)) (Val 4))
--192


-- Ejercicio 6
eval :: Expr -> Int
eval x = folde id (+) x

size :: Expr -> Int
size x = folde (\_ -> 1) (+) x


-- Ejercicio 7
data Maybe' a = Nothing' | Just' a

instance Eq a => Eq (Maybe' a) where
  (==) :: Maybe' a -> Maybe' a -> Bool
  (==) (Just' x) (Just' y) = x == y
  (==) Nothing' Nothing' = True
  (==) Nothing' (Just' _) = False
  (==) (Just' _) Nothing' = False

--data Maybe a = Nothing | Just a
data List' a = EndOfList | a ::: (List' a)
  deriving Show
infixr 0 :::

instance Eq a => Eq (List' a) where
  (==) :: List' a -> List' a -> Bool
  (==) EndOfList EndOfList = True
  (==) (x ::: xs) (y ::: ys) = (x == y) && (==) xs ys


-- Ejercicio 8
data Prop = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  | Or Prop Prop
  | Equi Prop Prop

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

eval2 :: Subst -> Prop -> Bool
eval2 _ (Const b) = b
eval2 s (Var x) = find x s
eval2 s (Not p) = not (eval2 s p)
eval2 s (And p q) = eval2 s p && eval2 s q
eval2 s (Imply p q) = eval2 s p <= eval2 s q
eval2 s (Or p q) = eval2 s p || eval2 s q
eval2 s (Equi p q) = (eval2 s (Imply p q)) && (eval2 s (Imply q p))

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equi p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval2 s p | s <- substs p]

p1 :: Prop
p1 = Imply (And (Var 'A') (Var 'B') ) (Or (Var 'A') (Var 'B') )

p2 :: Prop
p2 = Equi (And (Var 'A') (Var 'B') ) (Or (Var 'A') (Var 'B') )




-- Ejercicio 9

data Expr2 = Val2 Int | Add2 Expr2 Expr2 | Mult Expr2 Expr2

value :: Expr2 -> Int
value (Val2 n) = n
value (Add2 x y) = value x + value y
value (Mult x y) = value x * value y

type Cont = [Op]
data Op = EVAL Expr2 | ADD Int | MULT Int

eval3 :: Expr2 -> Cont -> Int
eval3 (Val2 n) c = exec c n
eval3 (Add2 x y) c = eval3 x (EVAL y : ADD 0 : c)
eval3 (Mult x y) c = eval3 x (EVAL y : MULT 1 :   c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : ADD m : c ) n = eval3 y (ADD  n : c)
exec (EVAL y : MULT m : c) n = eval3 y (MULT n : c)
exec (ADD n : c) m = exec c (n+m)
exec (MULT n : c) m = exec c (n*m)

value2 :: Expr2 -> Int
value2 e = eval3 e []


--value2 (Add2 (Mult (Val2 2) (Val2 3)) (Val2 4))
--10
--value2 (Mult (Add2 (Val2 2) (Mult (Val2 5) (Val2 10) )  ) (Val2 9)    )
--468
