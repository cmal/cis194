{-# LANGUAGE GeneralizedNewtypeDeriving
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , InstanceSigs
  #-}

module W07JoinList where

import W07Sized
import W07Buffer
import Data.Monoid
import W07StringBuffer
import W07Editor
-- import W07Scrabble

-- data JoinListBasic a = Empty
--                      | Single a
--                      | Append (JoinListBasic a) (JoinListBasic a)
                       
  
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty = []
-- jlbToList (Single a) = [a]
-- jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

-- Q1  

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m j1 j2) = m


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty j2 = j2
(+++) j1 Empty = j1
(+++) j1 j2 = Append (tag j1 <> tag j2) j1 j2



-- Q2
  
-- getSize :: Sized a => a -> Int
-- getSize = getSize . size

instance Sized (JoinList m a) where
  size Empty = Size 0
  size (Single _ _) = Size 1
  size (Append _ j1 j2) = size j1 <> size j2

indexJ :: (Sized b, Monoid b) => 
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n jl
  | n < 0 || n >= getSize jl = Nothing
  | otherwise = case jl of
                Single m x -> Just x
                Append _ j1 j2 ->
                  if n < getSize j1
                  then indexJ n j1
                  else indexJ (n - (getSize j1)) j2


(!!?) :: [a] -> Int -> Maybe a 
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 =Just x 
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

a = Append (Size 3)
      (Single (Size 1) 'y')
      (Append (Size 2)
        (Single (Size 1) 'e')
        (Single (Size 1) 'a'))
b = Single (Size 1) 'h'
t = Append (Size 4) a b


dropJ :: (Sized m, Monoid m) =>
         Int -> JoinList m a -> JoinList m a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ n jl@(Single _ _)
  | n < 0 = jl
  | otherwise = Empty
dropJ n jl@(Append m a b)
  | n < 0 = jl
  | n >= sizem = Empty
  | n >= sizea = dropJ (n - sizea) b
  | otherwise = (dropJ n a) +++ b
  where sizea = getSize a
        sizem = getSize m

takeJ :: (Sized m, Monoid m) =>
         Int -> JoinList m a -> JoinList m a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ n jl@(Single _ _)
  | n < 0 = Empty
  | otherwise = jl
takeJ n jl@(Append m a b)
  | n < 0 = Empty
  | n >= sizem = jl
  | n >= sizea = a +++ (takeJ (n - sizea) b)
  | otherwise = takeJ n a
  where sizea = getSize a
        sizem = getSize m


newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)  

instance Sized Score where
  size (Score x) = Size x

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty  = Score 0

scores = [
    ('a', 1), ('b', 3), ('c', 3), ('d', 2),
    ('e', 1), ('f', 4), ('g', 2), ('h', 4), ('i', 1), ('j', 8),
    ('k', 5), ('l', 1), ('m', 3),
    ('n', 1), ('o', 1), ('p', 3), ('q', 10), ('r', 1),
    ('s', 1), ('t', 1), ('u', 1), ('v', 4), ('w', 4),
    ('x', 8), ('y', 4), ('z', 10)
  ]

score :: Char -> Score
score c =
  case maybeS of
    Nothing -> Score 0
    (Just s) -> Score s
  where maybeS = lookup c scores

scoreString :: String -> Score
scoreString = fmap sum $ map score

scoreLine :: String -> JoinList Score String
scoreLine line = Single (scoreString line) line


-- Finally, combine these two kinds of annotations. A pair of monoids is
-- itself a monoid: instance (Monoid a, Monoid b) => Monoid (a,b) where
-- mempty = (mempty, mempty) mappend (a1,b1) (a2,b2) = (mappend a1 a2,
-- mappend b1 b2) (This instance is defined in Data.Monoid.) This means
-- that join-lists can track more than one type of annotation at once,
-- in parallel, sim- ply by using a pair type.  Since we want to track
-- both the size and score of a buffer, you should provide a Buffer
-- instance for the type JoinList (Score, Size) String.  Due to the use
-- of the Sized type class, this type will continue to work with your
-- functions such as indexJ.  Finally, make a main function to run the
-- editor interface using your join-list backend in place of the slow
-- String backend (see StringBufEditor.hs for an example of how to do
-- this). You should create an initial buffer of type JoinList (Score,
-- Size) String and pass it as an argument to runEditor editor. Verify
-- that the editor demonstration described in the section “Editors and
-- Buffers” does not exhibit delays when showing the prompt.

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--   mempty = (mempty, mempty)
--   mappend (a1,b1) (a2,b2) = (mappend a1 a2, mappend b1 b2)

buildLine :: String -> JoinList (Score, Size) String
buildLine line = Single (scoreString line, Size (length $ words line)) line


instance Buffer (JoinList (Score, Size) String) where
  -- | Convert a buffer to a String.
  toString :: JoinList (Score, Size) String -> String
  toString = unlines . jlToList

  -- | Create a buffer from a String.
  fromString :: String -> JoinList (Score, Size) String
  fromString s = foldr (+++) Empty $ map buildLine (lines s)

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine :: Int -> String
                     -> JoinList (Score, Size) String
                     -> JoinList (Score, Size) String
  replaceLine n ln buf
    | n < 1 = buf
    | n > getSize buf = buf
    | otherwise = takeJ (n - 1) buf +++ buildLine ln +++ dropJ n buf

  -- | Compute the number of lines in the buffer.
  numLines :: JoinList (Score, Size) String -> Int
  numLines = getSize

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value :: JoinList (Score, Size) String -> Int
  value Empty = 0
  value (Single (_, Size x) _) = x
  value (Append (_, Size x) _ _) = x

-- runEditor :: Buffer b => Editor b a -> b -> IO a  
main = runEditor editor jl
  where jl = (fromString content :: JoinList (Score, Size) String)
        content = unlines [ "This buffer is for notes you don't want to save, and for"
                          , "evaluation of steam valve coefficients."
                          , "To load a different file, type the character L followed"
                          , "by the name of the file."
                          ]

-- TODO NOTE: only type checked, not working now
