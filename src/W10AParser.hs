{-# LANGUAGE InstanceSigs #-}
{- CIS 194 HW 10
   due Monday, 1 April
-}

module W10AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

--  For example:

-- *Parser> runParser (satisfy isUpper) "ABC"
-- Just ('A',"BC")
-- *Parser> runParser (satisfy isUpper) "abc"
-- Nothing
-- *Parser> runParser (char 'x') "xyz"
-- Just ('x',"yz")



-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- runParser parser :: String -> Maybe (a, String)

-- (String -> Maybe (a, String)) -> (String -> Maybe (b, String))
-- fmap :: ((a, String) -> (b, String)) -> (Maybe (a, String) -> Maybe (b, String))
-- f : Maybe
-- fmap : fmap of Maybe Functor

-- fmap :: (a -> b) -> (a, String) -> (b, String)
-- fmap = first

-- fmap of Parser :: (a -> b) -> (String -> Maybe (a, String)) -> (String -> Maybe (b, String))

-- first f :: (a, String) -> (b, String)

-- ((a, String) -> (b, String)) -> Maybe (a, String) -> Maybe (b, String)
-- fmap

-- Maybe (a, String) -> Maybe (b, String)
-- fmap $ first f

-- (String -> Maybe (a, String)) -> (String -> Maybe (b, String))
-- fmap $ fmap $ first f

  
-- 反过来推
-- runParser parser :: String -> Maybe (a, String)

-- :t fmap first
-- fmap first :: Functor f => f (a -> b) -> f ((a, c) -> (b, c))
-- :t fmap . fmap first
-- fmap . fmap first :: Functor f => (a1 -> a2 -> b) -> f a1 -> f ((a2, c) -> (b, c))
-- :t fmap . fmap fmap first
-- fmap . fmap fmap first :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 (a, c)) -> f1 (f2 (b, c))
-- let g = (\c -> 1) :: Char -> Int
-- :t g
-- g :: Char -> Int
-- let first = (\f (a, c) -> (f a, c)) :: (a -> b) -> (a, c) -> (b, c)
-- :t first
-- first :: (a -> b) -> (a, c) -> (b, c)

-- :t (fmap . fmap fmap first) g $ runParser parser
-- (fmap . fmap fmap first) g $ runParser parser :: String -> Maybe (Int, String)

-- from the next homework's source code
inParser :: ((String -> Maybe (a1, String)) -> String -> Maybe (a2, String))
            -> Parser a1
            -> Parser a2
-- Parser :: (String -> Maybe (a, String)) -> Parser a
-- f :: (String -> Maybe (a1, String)) -> String -> Maybe (a2, String)
-- runParser :: Parser a -> String -> Maybe (a, String)
inParser f = Parser . f . runParser -- :: Parser a1 -> Parser a2

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  -- fmap f parser = Parser { runParser = (fmap . fmap fmap first) f $ runParser parser }
  -- using the IMPORTANT NOTE below about Record:
  fmap f (Parser a) = Parser $ (fmap . fmap fmap first) f a
  -- from the next homework's source code
  -- fmap = inParser . fmap . fmap . first

-- Exercise 2
-- Now implement an Applicative instance for Parser:
-- • pure a represents the parser which consumes no input and successfully returns a result of a.
-- • p1 <*> p2 represents the parser which first runs p1 (which will
-- consume some input and produce a function), then passes the
-- remaining input to p2 (which consumes more input and produces
-- some value), then returns the result of applying the function to the
-- value. However, if either p1 or p2 fails then the whole thing should
-- also fail (put another way, p1 <*> p2 only succeeds if both p1 and
-- p2 succeed).
-- So what is this good for? Recalling the Employee example from
-- class,
-- type Name = String
-- data Employee = Emp { name :: Name, phone :: String }
-- we could now use the Applicative instance for Parser to make an
-- employee parser from name and phone parsers. That is, if
-- parseName :: Parser Name
-- parsePhone :: Parser String
-- then
-- Emp <$> parseName <*> parsePhone :: Parser Employee
-- is a parser which first reads a name from the input, then a phone
-- number, and returns them combined into an Employee record. Of
-- course, this assumes that the name and phone number are right
-- next to each other in the input, with no intervening separators. We’ll
-- see later how to make parsers that can throw away extra stuff that
-- doesn’t directly correspond to information you want to parse.

-- !IMPORTANT!
-- NOTE: if we pattern matching (Parser g) = char 'c',
-- then g has type g :: String -> Maybe (Char, String)



instance Applicative Parser where
  pure x = Parser { runParser = \s -> Just (x, s)}
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  -- (Parser a2b) <*> (Parser b) should return Parser b
  (Parser x) <*> (Parser y) = Parser c
    where c s = case x s of
                  Nothing -> Nothing
                  Just (xx, s') -> case y s' of
                                       Nothing -> Nothing
                                       Just (yy, s'') -> Just (xx yy, s'')


-- Exercise 3
-- We can also test your Applicative instance using other simple
-- applications of functions to multiple parsers. You should implement
-- each of the following exercises using the Applicative interface to put
-- together simpler parsers into more complex ones. Do not implement
-- them using the low-level definition of a Parser! In other words, pretend that you do not have access to the Parser constructor or even
-- know how the Parser type is defined.
-- • Create a parser
abParser :: Parser (Char, Char)
-- -- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
-- abParser = (Parser a) <*> (Parser b)
--              where a ('a':s) = Just ((\b -> ('a', b)), s)
--                    a _ = Nothing
--                    b ('b':s) = Just ('b', s)
--                    b _ = Nothing
abParser = char 'a' *> pure (\b -> ('a', b)) <*> (char 'b' *> pure 'b')


-- which expects to see the characters ’a’ and ’b’ and returns them
-- as a pair. That is,
-- *AParser> runParser abParser "abcdef"
-- Just ((’a’,’b’),"cdef")
-- *AParser> runParser abParser "aebcdf"
-- Nothing
-- • Now create a parser
abParser_ :: Parser ()
abParser_ = char 'a' *> pure (\b -> ()) <*> (char 'b' *> pure 'b')

-- which acts in the same way as abParser but returns () instead of
-- the characters ’a’ and ’b’.


-- *AParser> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- *AParser> runParser abParser_ "aebcdf"
-- Nothing
-- • Create a parser intPair which reads two integer values separated
-- by a space and returns the integer values in a list. You should use
-- the provided posInt to parse the integer values.
-- *Parser> runParser intPair "12 34"
-- Just ([12,34],"")

intPair :: Parser [Integer]
-- intPair = (Parser a) <*> (char ' ') <*> posInt
--           where a s = case runParser posInt s of
--                         Nothing -> Nothing
--                         Just (i, s') -> Just ((\w -> \j -> [i, j]), s')
intPair = (\i w j -> [i, j]) <$> posInt <*> (char ' ') <*> posInt

-- Exercise 4
-- Applicative by itself can be used to make parsers for simple, fixed
-- formats. But for any format involving choice (e.g. “. . . after the colon
-- there can be a number or a word or parentheses. . . ”) Applicative is
-- not quite enough. To handle choice we turn to the Alternative class,
-- defined (essentially) as follows:
-- class Applicative f => Alternative f where
-- empty :: f a
-- (<|>) :: f a -> f a -> f a
-- (<|>) is intended to represent choice: that is, f1 <|> f2 represents
-- a choice between f1 and f2. empty should be the identity element for
-- (<|>), and often represents failure.
-- Write an Alternative instance for Parser:
-- • empty represents the parser which always fails.
-- • p1 <|> p2 represents the parser which first tries running p1. If
-- p1 succeeds then p2 is ignored and the result of p1 is returned.
-- Otherwise, if p1 fails, then p2 is tried instead.
-- Hint: there is already an Alternative instance for Maybe which you
-- may find useful.


-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a


-- instance Alternative Maybe where
--   empty = Nothing
--   Nothing <|> p = p
--   Just x <|> _ = Just x
     
-- empty = undefined 
-- (<|>) = undefined

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser x
    where x s = case a s of
                  Nothing -> b s
                  _ -> a s



-- Exercise 5
-- Implement a parser

intParser :: Parser ()
-- intParser = Parser go
--               where go s = case runParser posInt s of
--                              Just (_, s') -> Just ((), s')
--                              _ -> Nothing
intParser = const <$> pure () <*> posInt

upperParser :: Parser ()
-- upperParser = Parser { runParser = go }
--                 where go s = case runParser (satisfy isUpper) s of
--                                Just (_, s') -> Just ((), s')
--                                _ -> Nothing
upperParser = const <$> pure () <*> (satisfy isUpper)

intOrUppercase :: Parser ()
intOrUppercase = intParser <|> upperParser
-- which parses either an integer value or an uppercase character, and
-- fails otherwise.
-- *Parser> runParser intOrUppercase "342abcd"
-- Just ((), "abcd")
-- *Parser> runParser intOrUppercase "XYZ"
-- Just ((), "YZ")
-- *Parser> runParser intOrUppercase "foo"
-- Nothing
-- Next week, we will use your parsing framework to build a more
-- sophisticated parser for a small programming language!
