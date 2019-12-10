module Golf where

-- Rules refer to:
-- https://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf

import Data.Map.Strict (Map, empty, insertWith)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)


-- Q1

-- takeNth, get the Nth member out of a List
g :: Int -> [a] -> [a]
g m = map snd . filter ((== m) . fst) . zip (cycle [1..m])

skips :: [a] -> [[a]]
skips x = map (\y -> g y x) [1..length x]

-- Q2

localMaxima :: [Integer] -> [Integer]
-- rename localMaxima to shorten the code
f :: [Integer] -> [Integer]
f (x:a@(y:z:b))
  | y >= x && y >= z = y:f a
  | True = f a
f _ = []
localMaxima = f

-- Q3

-- print True to '*', False to ' '
p :: Bool -> Char
p True = '*'
p _ = ' '

-- calc max count, then map l to the max count,
-- then add all the lines together.
histogram :: [Integer] -> String
histogram x =
  unlines (map
           -- print line
           (\i -> map
             -- get '*' or ' ' from the Map line by line,
             -- `fromMaybe 0 (M.lookup j m)` lookup the Map with 0 as default
             (\j -> (p $ fromMaybe 0 (M.lookup j m) >= i))
             -- char by char, for each line
             [0..9])
            -- from top to bottom
           $ reverse [1..M.foldr max 0 m])
  -- suffix
  ++ "==========\n0123456789\n"
  -- construct the Map, key: number, value: occurrencies/frequencies
  where m = foldl (\m k -> insertWith (+) k 1 m) empty x

