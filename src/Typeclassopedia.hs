# LANGUAGE InstanceSigs #
module Typeclassopedia where

-- exercises on haskell wiki typeclassopedia
import Control.Applicative


data Pair a = Pair a a

instance Functor Pair where -- do not change the container
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair x y) = Pair (f x) (f y)

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
  fmap :: (a -> b) -> ITree a -> ITree b
  fmap g (Leaf f) = Leaf $ g . f
  fmap g (Node ts) = Node $ fmap (fmap g) ts

-- 4. Give an example of a type of kind * -> * which cannot be made an instance of Functor 
-- https://stackoverflow.com/questions/16118414/an-example-of-a-type-with-kind-which-cannot-be-an-instance-of-functor


-- 5. The composition of two Functors is also a Functor.
-- True
-- https://stackoverflow.com/questions/14014982/composition-of-two-functors-is-a-functor


-- instance Applicative Maybe where
--   pure = Just
--   Nothing <*> _ = Nothing
--   Just f <*> x = fmap f x


-- newtype ZipList a = ZipList { getZipList :: [a] }

-- instance Applicative ZipList where
--   pure :: a -> ZipList a
--   pure = ZipList . repeat

--   (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
--   (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)


sequenceAL :: Applicative f => [f a] -> f [a]
-- sequenceAL [] = pure []
-- sequenceAL (x:xs) = fmap (\x -> foldr collect x xs) wrap x
--            where wrap x = (fmap (:) x <*> pure [])
--                  collect x y = fmap (++) ... 不对
-- sequenceAL = foldr (\x y -> (:) <$> x <*> y) (pure [])
sequenceAL = foldr (liftA2 (:)) (pure [])


-- sequenceALMaybe :: [Maybe a] -> Maybe [a]
-- sequenceALMaybe [] = Nothing
-- sequenceALMaybe (x:xs) = pure $ foldr (++) xs (fmap (:) x <*> pure [])

-- (fmap (:) (Just 1) <*> pure []) :: Num a => Maybe [a]
-- fmap (\x -> foldr (++) x []) (Just [1]) :: Num a => Maybe [a]



-- some practice on the Class notes of Week 11

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (flip const)

-- sequenceA :: Applicative f => [f a] -> f [a]
-- sequenceA = foldr (liftA2 (:)) (pure [])

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f xs = sequenceA (map f xs)


replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n x = sequenceA $ take n $ repeat x



-- 1. Implement a Monad instance for the list constructor, []. Follow the types!
-- 2. Implement a Monad instance for ((->) e).
-- 3. Implement Functor and Monad instances for Free f, defined as
-- data Free f a = Var a
--               | Node (f (Free f a))
-- You may assume that f has a Functor instance. This is known as the free monad built from the functor f.

instance Monad [] where
  return = pure
  (>>=) ma f = concat $ map f ma


instance Monad ((->) e) where
  return = pure
  (>>=) ma f = ma f mempty


data Free f a = Var a
              | Node (f (Free f a))

instance Functor f => Monad (Free f) where
  return = undefined
  (>>=) = undefined
