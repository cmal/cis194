module W11AParser (Parser, runParser, satisfy, char, posInt) where

import           Control.Applicative
import           Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

inParser f = Parser . f . runParser

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap = inParser . fmap . fmap . first

instance Applicative Parser where
  pure x = Parser { runParser = \s -> Just (x, s)}
  (Parser x) <*> (Parser y) = Parser c
    where c s = case x s of
                  Nothing -> Nothing
                  Just (xx, s') -> case y s' of
                                       Nothing -> Nothing
                                       Just (yy, s'') -> Just (xx yy, s'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser x
    where x s = case a s of
                  Nothing -> b s
                  _ -> a s
