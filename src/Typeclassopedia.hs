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



class Foldable t where
  fold    :: Monoid m => t m -> m
  fold = foldMap id -- 1. Implement fold in terms of foldMap.
  -- 2. What would you need in order to implement foldMap in terms of fold?
  foldMap :: Monoid m => (a -> m) -> t a -> m  
  -- 3. Implement foldMap in terms of foldr.
  foldMap f = foldr (mappend . f) mempty -- stolen from GHC source code, Foldable.hs
  foldr   :: (a -> b -> b) -> b -> t a -> b
  -- 4. Implement foldr in terms of foldMap (hint: use the Endo monoid).
  foldr f z t = appEndo (foldMap (Endo #. f) t) z -- stolen from GHC source code, Foldable.hs
  foldr'  :: (a -> b -> b) -> b -> t a -> b
  foldl   :: (b -> a -> b) -> b -> t a -> b
  foldl'  :: (b -> a -> b) -> b -> t a -> b
  foldr1  :: (a -> a -> a) -> t a -> a
  foldl1  :: (a -> a -> a) -> t a -> a
  toList  :: t a -> [a]
  null    :: t a -> Bool
  length  :: t a -> Int
  elem    :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum     :: Num a => t a -> a
  product :: Num a => t a -> a

  -- 5. What is the type of foldMap . foldMap? Or foldMap . foldMap . foldMap, etc.? What do they do?
  foldMap . foldMap
  -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  -- (.) :: (b -> c) -> (a -> b) -> a -> c
  -- foldMap . foldMap :: 
  -- b in (.) :: a -> m
  -- c in (.) :: t a -> m
  -- a in (.) :: a -> m
  -- second b in (.) :: t1 a -> m

  -- so first b in (.) :: t2 a -> m
  -- c in (.) :: t1 (t2 a) -> m
  -- a -> c in (.) :: (a -> m) -> t1 (t2 a) -> m

  -- from GHCi
  foldMap . foldMap
  :: (Monoid m, Foldable t1, Foldable t2) =>
     (a -> m) -> t1 (t2 a) -> m

  foldMap . foldMap . foldMap
  :: (Monoid m, Foldable t1, Foldable t2, Foldable t3) =>
     (a -> m) -> t1 (t2 (t3 a)) -> m
  -- What do they do? for example
  -- ( (foldMap . foldMap . foldMap) (:) [[[1],[2]],[[3]]]) []
