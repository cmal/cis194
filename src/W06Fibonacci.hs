{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module W06Fibonacci where

import Data.List

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (pred n) Prelude.+ fib (pred $ pred n)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 1:1:(go 2)
            where go n = (fibs2!!(pred n) Prelude.+ fibs2!!(pred $ pred n)):(go (succ n))


data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x:(streamToList s)

instance Show a => Show (Stream a) where
  show :: Show a => Stream a -> String
  show s = intercalate "," (map Prelude.show $ take 20 $ streamToList s) ++ ",..."


streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))
                             
               
nats :: Stream Integer
nats = streamFromSeed succ 0


-- ruler :: Stream Integer

rule :: Integer -> Integer
rule n
  | odd n = 0
  | otherwise = succ $ rule $ quot n 2

ruler :: Stream Integer
ruler = streamMap rule t
          where (Cons _ t) = nats

-- Q6

x :: Stream Integer
x = Cons 0 (Cons 1 $ streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (+) (Cons x1 s1) (Cons x2 s2) = Cons (x1 + x2) (s1 + s2) 
  (*) (Cons x1 r1) s2@(Cons x2 r2) = Cons (x1 * x2) $ (streamMap (* x1) r2) + r1 * s2
