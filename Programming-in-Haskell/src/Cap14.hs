module Cap14 where

import Data.Monoid
import Data.Foldable

-- Ejercicio 1
class Monoid' a where
  mempty' :: a
  mappend' :: a -> a -> a
  mconcat' :: [a] -> a
  mconcat' = foldr mappend' mempty'


instance (Monoid' a, Monoid' b) => Monoid' (a,b) where
  -- mempty' :: (a,b)
  mempty' = (mempty', mempty')
  -- mappend' :: (a,b) -> (a,b) -> (a,b)
  (x1,y1) `mappend'`  (x2,y2) = ((x1 `mappend'` x2), (y1 `mappend'` y2))



-- Ejercicio 2

instance (Monoid' b) => Monoid' (a -> b) where
  -- mempty' :: (a -> b)
  mempty' = (\inp -> mempty')
  -- mappend' :: (a -> b) -> (a -> b) -> (a -> b)
  f `mappend'` g = (\x -> (f x) `mappend'` (g x))


-- Ejercicio 3

class Foldable' t where
  fold' :: Monoid a => t a -> a
  foldMap' :: Monoid b => (a -> b) -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl' :: (a -> b -> a) -> a -> t b -> a

instance Foldable' (Maybe) where
  -- fold' :: Monoid a => t a -> a
  fold' (Just x) = x
  fold' (Nothing) = mempty
  --foldMap' :: Monoid b => (a -> b) -> t a -> b
  foldMap' f Nothing = mempty
  foldMap' f (Just x) = f x
  --foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' f v Nothing = v
  foldr' f v (Just x) = f x v
  --foldl' :: (a -> b -> a) -> a -> t b -> a
  foldl' f v Nothing = v
  foldl' f v (Just x) = f v x

class (Functor t, Foldable t) => Traversable' t where
  traverse' :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable' Maybe where
  --traverse' :: Applicative f => (a -> f b) -> t a -> f (t b)
  -- t es Maybe
  traverse' g (Nothing) = pure (Nothing)
  traverse' g (Just x) =  pure Just <*> g x 
  -- x :: a
  -- g x :: f b
  -- pure Just :: f (b -> t b)
  -- pure Just <*> g x :: f (t b)
  


-- Ejercicio 4
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Foldable Tree where
  -- fold :: Monoid a => t a -> a
  fold (Leaf) = mempty
  fold (Node l i r) = (fold l) `mappend` i `mappend` (fold r)
  -- foldMap :: Monoid b => (a -> b) -> t a -> b
  foldMap g (Leaf) = mempty
  foldMap g (Node l i r) = (g i) `mappend` (foldMap g l) `mappend` (foldMap g r)
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr g v (Leaf) = v
  foldr g v (Node l i r) = g i (foldr g (foldr g v r) l)
  -- foldl :: (a -> b -> a) -> a -> t b -> a
  foldl g v (Leaf) = v
  foldl g v (Node l i r) = g (foldl g (foldl g v l) r) i

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l i r) = Node (fmap g l) (g i) (fmap g r)

instance Traversable Tree where
  --traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse g (Leaf) = pure Leaf
  traverse g (Node l i r) = pure Node <*> traverse g l <*> g i <*> traverse g r
     
  
-- Ejercicio 5
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF g = foldMap (\x -> if g x then [x] else mempty) 

-- filterF even (Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf))
