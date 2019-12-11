module W04HigherOrder where

-- Q1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate (\n ->
                                                         if even n
                                                         then div n 2
                                                         else 1 + n * 3)


-- f 8 : 8 4 2 1

-- f 5 : 5 16 8 4 2 1
--         ^  ^ ^ ^  sum = 30

-- 1  0
-- 2  2
-- 3  f 10 = 40
-- 4  4 + f 2 = 6
-- 5  f 16 = 30
-- 6  6 + f 3 = 46 
-- 7  f 22 = 234
-- 8  8 + f 4 = 14
-- 9
-- 10 10 + f 5 = 40
-- 11 f 34 = 212
-- 12
-- 13 f 40 = 100
-- 14
-- 15
-- 16 16 + f 8 = 30
-- 17 f 52 = 178
-- 18
-- 19
-- 20 20 + f 10 = 60
-- 21
-- 22 22 + f 11 = 234
-- 25
-- 26 26 + f 13 = 126
-- 27
-- 28
-- 29
-- 30
-- 31
-- 32
-- 33
-- 34 34 + f 17 = 212

-- 40 40 + f 20 = 100

-- 52 52 + f 26 = 178



-- Q2

data Tree a = Leaf
  | Node Int (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Int
height Leaf = -1
height (Node h _ _ _) = h

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node _ left node right)
  | hl <= hr  = Node (succ $ max nhl hr) newLeft node right
  | otherwise = Node (succ $ max hl nhr) left node newRight
  where hl = height left
        hr = height right
        newLeft = insertTree x left
        newRight = insertTree x right
        nhl = height newLeft
        nhr = height newRight

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf


-- foldTree "ABCDEFGHIJ" ==
--   Node 3
--   (Node 2
--    (Node 0 Leaf 'F' Leaf)
--    'I'
--    (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
--   'J'
--   (Node 2
--    (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
--    'H'
--    (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))



-- Q3
   
xor :: [Bool] -> Bool
xor = not . even . foldr (\i n -> if i then succ n else n) 0


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x:ys) []


-- 3. (Optional) Implement foldl using foldr. That is, complete the
-- definition
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr ...
-- in such a way that myFoldl behaves identically to the standard
-- foldl function.
-- Hint: Study how the application of foldr and foldl work out:
-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr (\x y -> )

myFoldl f base xs = foldr (flip f) base $ (reverse xs)

-- Q4


-- Exercise 4: Finding primes
-- Read about the Sieve of Sundaram. Implement the algorithm us- 
-- function composition. Given an integer n, your function should
-- generate all the odd prime numbers up to 2n + 2.
-- sieveSundaram :: Integer -> [Integer]
-- sieveSundaram = ...
-- To give you some help, below is a function to compute the Cartesian
-- product of two lists. This is similar to zip, but it produces all
-- possible pairs instead of matching up the list elements. For example,
-- cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
-- It’s written using a list comprehension, which we haven’t talked about
-- in class (but feel free to research them).

-- cartProd :: [a] -> [b] -> [(a, b)]
-- cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = takeWhile (<= end) (map ((+ 1) . (* 2)) (filter sieve [1..end]))
                  where end = 2 * n + 2      
                        sieve = \x -> not $ elem x $ [y |
                                                      i <- [1..end],
                                                      j <- [1..end],
                                                      1 <= i,
                                                      i <= j,
                                                      let y = i + j + 2 * i * j,
                                                          y <= end]
