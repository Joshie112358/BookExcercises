module Cap15 where


-- Ejerciicio 1
-- 1 + (2*3)  outermost
-- (2*3)      innermost

-- (1+2) * (2+3)   outermost
-- (1+2)           innermost (leftmost)
-- (2+3)           neither

-- fst (1+2, 2+3)   outermost
-- 1+2              innermost
-- 2+3              neither
-- (1+2, 2+3)       neither

-- (\x -> 1 + x) (2*3)   outermost
-- (2*3)                 neither
-- (\x -> 1 + x)         innermost / neither


-- Ejercicio 2
-- fst (1+2, 2+3)
-- Es mejor outermost porque asi no gastamos recursos evaluando 2+3 que al final no se usaria

-- Ejercicio 3
-- mult :: Int -> Int -> Int
-- mult = \x -> (\y -> x * y)


-- mult 3 4 =
-- (aplicando mult)
-- \x -> (\y -> x * y) 3 4 =
-- (aplicando 1ra lambda)
-- (\y -> 3 * y) 4 =
-- (aplicando lambda)
-- 3 * 4 =
-- (aplicando *)
-- 12


-- Ejercicio 4
fibs :: [Integer]
fibs = [0] ++ [1] ++ [x+y | (x,y) <- zip (fibs) (tail fibs)  ]

-- Ejercicio 5
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

repeatt :: a -> Tree a
repeatt x = Node xs x xs where xs = repeatt x

taket :: Int -> Tree a -> Tree a
taket 0 _ = Leaf
taket _ Leaf = Leaf
taket n (Node l i r) = Node (taket (n-1) l) i (taket (n-1) r)

replicatet :: Int -> a -> Tree a
replicatet n = taket n . repeatt


-- Ejercicio 6
sqroot :: Double -> Double
sqroot n = head [x | (y,x) <- zip (posroots n) (tail (posroots n)), abs(y-x) < 0.00001 ]

posroots :: Double -> [Double]
posroots n = iterate (\x -> (x+(n/x))/2 ) (n/2)
