module W08Party where

import W08Employee
import Data.Tree

-- Q1

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) (fun + empFun emp)

instance Semigroup GuestList where
  (GL emps1 fun1) <> (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max



-- Q2

-- tree is not empty
-- data Tree is defined as: Non-empty, possibly infinite, multi-way trees;
-- also known as rose trees.
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f = go
  where go (Node x ts) = f x (map go ts)

-- combineGLs :: Employee -> [GuestList] -> GuestList
-- combineGLs emp gls = pending


-- Q3

-- However, this obvious first attempt fails! The problem is that we
-- don’t get enough information from the recursive calls. If the best
-- guest list for some subtree involves inviting that subtree’s boss, then
-- we are stuck, since we might want to consider inviting the boss of the
-- entire tree—in which case we don’t want to invite any of the subtree
-- bosses (since they wouldn’t have any fun anyway). But we might be
-- able to do better than just taking the best possible guest list for each
-- subtree and then excluding their bosses.
-- The solution is to generalize the recursion to compute more information,
-- in such a way that we can actually make the recursive step.
-- In particular, instead of just computing the best guest list for a given
-- tree, we will compute two guest lists:
-- 1. the best possible guest list we can create if we invite the boss (that
-- is, the Employee at the root of the tree); and
-- 2. the best possible guest list we can create if we don’t invite the boss.
-- It turns out that this gives us enough information at each step to
-- compute the optimal two guest lists for the next level up.


nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp glPairs = ( glCons emp $ foldr (<>) mempty (map snd glPairs)
                        , foldr (<>) mempty (map (\pair -> max (fst pair) (snd pair)) glPairs))


-- which takes two arguments. The first is the “boss” of the current subtree
-- (let’s call him Bob). The second argument is a list of the results
-- for each subtree under Bob. Each result is a pair of GuestLists: the
-- first GuestList in the pair is the best possible guest list with the boss
-- of that subtree; the second is the best possible guest list without the
-- boss of that subtree. nextLevel should then compute the overall best
-- guest list that includes Bob, and the overall best guest list that doesn’t
-- include Bob.


-- Q4

maxFun :: Tree Employee -> GuestList
maxFun t = moreFun (fst res) (snd res)
  where res = treeFold nextLevel t


-- Q5
-- Implement main :: IO () so that it reads your company’s hierarchy
-- from the file company.txt, and then prints out a formatted guest
-- list, sorted by first name, which looks like
-- Total fun: 23924
-- Adam Debergues
-- Adeline Anselme
-- ...
-- (Note: the above is just an example of the format; it is not the correct
-- output!) You will probably find the readFile and putStrLn functions
-- useful.
-- As much as possible, try to separate out the “pure” computation
-- from the IO computation. In other words, your main function should
-- actually be fairly short, calling out to helper functions (whose types
-- do not involve IO) to do most of the work. If you find IO “infecting”
-- all your function types, you are Doing It Wrong.

prn :: GuestList -> String
prn (GL emps fun) = unlines $ [ "Total fun: " ++ show fun ] ++ map empName emps

main :: IO ()
main = do
  contents <- readFile "./company.txt"
  putStrLn $ prn $ maxFun $ read contents
