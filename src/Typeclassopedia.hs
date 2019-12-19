module Typeclassopedia where

-- exercises on haskell wiki typeclassopedia


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
sequenceAL [] = pure []
-- sequenceAL (x:xs) = fmap (\x -> foldr collect x xs) wrap x
--            where wrap x = (fmap (:) x <*> pure [])
--                  collect x y = fmap (++) ... 不对
sequenceAL xs = foldr (\x y -> (:) <$> x <*> y) (pure []) xs

-- sequenceALMaybe :: [Maybe a] -> Maybe [a]
-- sequenceALMaybe [] = Nothing
-- sequenceALMaybe (x:xs) = pure $ foldr (++) xs (fmap (:) x <*> pure [])

-- (fmap (:) (Just 1) <*> pure []) :: Num a => Maybe [a]
-- fmap (\x -> foldr (++) x []) (Just [1]) :: Num a => Maybe [a]
