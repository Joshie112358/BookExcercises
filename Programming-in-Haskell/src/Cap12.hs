module Cap12 where
{-
Functores
Clase que acepta funciones (fmap) que apliquen una funcion a todos los elementos de la estructura.

class Functor f where
fmap :: (a -> b) -> f a -> f b

Applicatives
Clase que generalisa la idea de functores, aceptan funciones que pueden tomar distintos numeros
de argumentos que se apliquen a todos los elementos de la estructura.

class Functor f => Applicative f where
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b

fmap0 :: a -> f a
fmap0 = pure
fmap1 :: (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y
fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g x y z = pure g <*> x <*> y <*> z


Monads



class Applicative m => Monad m where
return :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b
return = pure

return :: Applicative f => a -> f a
return = pure


-}


----------------------------------------------------------------
-- Ejercicio 1

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor (Tree) where
  --fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap g (Node lb x rb) = Node (fmap g lb) (g x) (fmap g rb)


-- Ejercicio 2 ----------------------------------------------------

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' ((->) a) where
  --fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap' g h = g.h

data Pair' a b = CPair' a b
instance Functor (Pair' a) where
  --fmap :: (b -> c) -> (Pair' a b) -> (Pair' a c)
  fmap g (CPair' a b) = CPair' a (g b)

  

-- Ejercicio 3 -----------------------------------------------------
class Functor f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b


instance Applicative' ((->) a) where
  -- pure' :: b -> (a -> b)
  pure' y = (\_ -> y)
  -- (<**>) :: f (a -> b) -> f a -> f b
  -- (<**>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  -- (<**>) :: Pair' a (b -> c) -> (Pair' a b) -> (Pair' a c)
  -- (<**>) :: (->) a (b -> c) -> (a -> b) -> (a -> c)
  -- (<**>) :: (a -> (b -> c)) -> (a -> b) -> a -> c
  f <**> g = \x -> (f x) (g x)



-- Ejercicio 4 ------------------------------------------------
newtype ZipList a = Z [a]
  deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)
    
instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z [x | _ <- [1..]]
  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [f a | (f,a) <- zip gs xs]

-- let x = Z [(+1),(+2),(+3)]
-- let

-- Ejercicio 5 -------------------------------------------------
{-
Applicative laws


1.-
pure id <*> x = x
((pure id) <*> x) = x
id :: a -> a
pure id :: f (a -> a)
x :: f a
((pure id) <*> x) :: f a



2.-
pure (g x) = pure g <*> pure x
pure g :: f (a -> b)
pure x :: f a
g :: a -> b
x :: a
g x :: b
pure (g x) :: f b
pure g <*> pure x :: f b



3.-
x <*> pure y = pure (\g -> g y) <*> x
x :: f (a -> b)
pure y :: f a
y :: a
x <*> pure y :: f b
--------------------------------------
pure (\g -> g y) :: f ((a -> b) ->  b)
(\g -> g y) :: (a -> b) -> b
g :: a -> b
pure (\g -> g y) <*> x :: f b



4.-
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
x :: f (b -> c)
(y <*> z) = f b
y :: f (a -> b)
z :: f a
x <*> (y <*> z) :: f c
------------------------------------
(pure (.) <*> x <*> y) :: f (a -> c)
pure (.) <*> x :: f ((a -> b) -> (a -> c) )
pure (.) :: f ( (b -> c) -> ((a -> b) -> (a -> c)))
(.) :: ( (b -> c) -> ((a -> b) -> (a -> c)))


-}


    

-- Ejercicio 6 -----------------------------------------------------
class Applicative m => Monad' m where
  return' :: a -> m a
  (>>>=) :: m a -> (a -> m b) -> m b
  return' = pure

instance Monad' ((->) a) where
  -- (>>>=) :: (Pair' a b) -> (b -> Pair' a c) -> (Pair' a c)
  -- (>>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
  f >>>= g = \x -> (g (f x)) x


-- Ejercicio 7 ------------------------------------------------

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  --fmap  :: (a -> b) -> Expr a -> Expr b
  fmap _ (Val x) = Val x
  fmap g (Var x) = Var (g x)
  fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
  --pure :: a -> Expr a
  pure a = Var a
  -- <*> :: Expr (a -> b) -> Expr a -> Expr b
  (Var g) <*> ex = fmap g ex
  _ <*> Val ex = Val ex
  g <*> Add x y = Add (g <*> x) (g <*> y)
  --(Add x y) <*> ex = 

instance Monad Expr where
  -- (>>=) :: (Expr a) -> (a -> Expr b) -> Expr b
  (Var mx) >>= f = f mx
  (Val x) >>= _ = Val x
  (Add x y) >>= f = Add (x >>= f) (y >>= f)

ex :: Expr [Int]
ex = (Add (Var [1,2,3] ) (Var [5]))

testfunc :: [Int] -> Expr Int
testfunc xs = pure (2)

-- ex >>= testfunc
-- Add (Var 3) (Val 5)
-- >>= cambia el tipo a de (Expr a) a tipo b en (Expr b) con ayuda de la funcion que se le proporciona
  
-- Ejercicio 8 --------------------------------------
type State = Int
newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do
    st' <- st
    pure (g st')
    --(\a -> pure (g a))
    
instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x,s))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    stf' <- stf
    stx' <- stx
    pure (stf' stx')
    --stx >>= (\a -> S (\s -> ( (fst (app stf s)) a ,s )   ) )
  --a :: a
  --s :: State
  --fst (app stf s) :: (a -> b)
  --(fst (app stf s)) a :: b
  -- S (\s -> ( (fst (app stf s)) a ,s )   ) :: ST b
  -- (\a -> S (\s -> ( (fst (app stf s)) a ,s )   ) ) :: a -> ST b
    
instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s ->
                  let (x,s') = app st s in app (f x) s')
