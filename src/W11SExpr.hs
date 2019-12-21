{- CIS 194 HW 11
   due Monday, 8 April
-}

module W11SExpr where

import W11AParser -- (Parser, runParser, satisfy, char, posInt)
import Data.Char
import Control.Applicative


-- Remember, for this week’s homework you should only need to
-- write code on top of the interface provided by the Functor, Applicative,
-- and Alternative instances. In particular, you should not write any
-- code that depends on the details of the Parser implementation. (To
-- help with this, the version of AParser.hs we provided this week does
-- not even export the Parser constructor, so it is literally impossible to
-- depend on the details!)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = oneOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show, Eq)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show, Eq)

parseAtom :: Parser SExpr
parseAtom = fmap A $ fmap N posInt <|> fmap I ident

parseSExpr :: Parser SExpr
parseSExpr = parseAtom <|>
               zeroOrMore spaces *> (char '(') *>
               (fmap Comb $ oneOrMore (zeroOrMore spaces *> parseAtom <|> parseSExpr))
               <* zeroOrMore spaces <* (char ')')
