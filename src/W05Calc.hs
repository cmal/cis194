{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module W05Calc where

import W05ExprT
import W05Parser
import qualified Data.Map as M
import qualified W05StackVM as VM

-- Q1

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Add e1 e2) = (eval e1) + (eval e2)

-- Q2

evalStr :: String -> Maybe Integer
evalStr x = eval <$> (parseExp Lit Add Mul x)


-- Q3


class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul


-- Notice: this function constrains the input type,
-- made the input from Expr a => a to ExprT
reify :: ExprT -> ExprT
reify = id
  

-- Q4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = lit $ max x y
  mul (MinMax x) (MinMax y) = lit $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 $ mod x 7
  add (Mod7 x) (Mod7 y) = lit $ (+) x y
  mul (Mod7 x) (Mod7 y) = lit $ (*) x y


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


-- Q5


-- String -> Maybe [StackExp]


instance Expr VM.Program where
  lit x = [VM.PushI x]
  add x y = x ++ y ++ [VM.Add]
  mul x y = x ++ y ++ [VM.Mul]
  

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul


-- Q6

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

type MMap = (M.Map String Integer -> Maybe Integer)

-- var :: String -> MMap

instance HasVars MMap where
  var = M.lookup

-- lit :: Integer -> MMap
-- add :: MMap -> MMap -> MMap

instance Expr MMap where
  lit x = \_ -> Just x
  add a b = \x -> (+) <$> (a x) <*> (b x)
  mul a b = \x -> (*) <$> (a x) <*> (b x)

withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
