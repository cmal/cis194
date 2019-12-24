{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

-- type Rand g = RandT g Identity
-- data RandT g m a
-- g - The type of generator.
-- m - The type of inner monad.
-- a - The type of return value
die :: Rand StdGen DieValue
--  getRandom :: (MonadRandom m, (Random a)) => m a
die = getRandom

-- type Rand g = RandT g Identity
-- A random monad parameterized by the type g of the generator to carry.
-- here m : Rand StdGen is a Monad instance type
-- so, type DieValue, is a Random instance type, SEE: line 16
-- Rand StdGen is a MonadRandom

-- class Monad m => MonadRandom m where

-- With a source of random number supply in hand, the MonadRandom
-- class allows the programmer to extract random values of a variety
-- of types.

-- Minimal complete definition
-- getRandomR, getRandom, getRandomRs, getRandoms

-- SEE: https://hackage.haskell.org/package/MonadRandom-0.5.1.1/docs/src/Control.Monad.Random.Class.html#line-205
-- instances: (but not found the source code)
-- (RandomGen g, Monad m) => MonadRandom (RandT g m)
-- so the RandT g m is an instance of class MonadRandom
-- so type Rand g is an instance of class MonadRandom
-- so type Rand g defined the 4 methods of class MonadRandom
-- so Rand StdGen has method getRandom, i.e. the `die` method



------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Eq, Show)



-- Exercise 1
-- Type cabal install MonadRandom at a command prompt (not the ghci
-- prompt) to download and install the MonadRandom package from
-- Hackage. Then visit the documentation (http://hackage.haskell.
-- org/package/MonadRandom). Take a look at the Control.Monad.Random
-- module, which defines various ways to “run” a Rand computation; in
-- particular you will eventually (at the very end of the assignment)
-- need to use the evalRandIO function. Take a look also at the
-- Control.Monad.Random.Class module, which defines a MonadRandom
-- class containing methods you can use to access the random generator
-- in a Rand computation. For example, this is where the getRandom
-- function (used above in the threeInts example) comes from. However,
-- you probably won’t need to use these methods directly in this
-- assignment.  In Risk.hs we have provided a type newtype DieValue =
-- DV { unDV :: Int } for representing the result of rolling a
-- six-sided die. We have also provided an instance of Random for
-- DieValue (allowing it to be used with MonadRandom), and a
-- definition die :: Rand StdGen DieValue die = getRandom which
-- represents the random outcome of rolling a fair six-sided die.

-- evalRandIO die :: IO DieValue



-- The Rules 

-- The rules of attacking in Risk are as follows. 

-- • There is an attacking army (containing some number of units) and
-- a defending army (containing some number of units).

-- • The attacking player may attack with up to three units at a time.
-- However, they must always leave at least one unit behind. That is,
-- if they only have three total units in their army they may only
-- attack with two, and so on.
-- 最多使用3个单位攻击，但至少要留一个单位不行动

-- • The defending player may defend with up to two units (or only one
-- if that is all they have).
-- 至多使用两个单位防御

-- • To determine the outcome of a single battle, the attacking and
-- defending players each roll one six-sided die for every unit they
-- have attacking or defending. So the attacking player rolls one,
-- two, or three dice, and the defending player rolls one or two dice.
-- 攻击方摇1-3次，防御方摇1-2次

-- • The attacking player sorts their dice rolls in descending
-- order. The defending player does the same.
-- 以逆序排列摇出的数字

-- • The dice are then matched up in pairs, starting with the highest
-- roll of each player, then the second-highest.
-- 比较

-- • For each pair, if the attacking player’s roll is higher, then one of
-- the defending player’s units die. If there is a tie, or the defending
-- player’s roll is higher, then one of the attacking player’s units die.
-- For example, suppose player A has 3 units and player B has 5. A
-- can attack with only 2 units, and B can defend with 2 units. So A
-- rolls 2 dice, and B does the same. Suppose A rolls a 3 and a 5, and B
-- rolls a 4 and a 3. After sorting and pairing up the rolls, we have
-- 攻击方数字高时，防御方死亡，否则，攻击方死亡
-- A B
-- 5 4
-- 3 3
-- A wins the first matchup (5 vs. 4), so one of B’s units dies. The
-- second matchup is won by B, however (since B wins ties), so one of
-- A’s units dies. The end result is that now A has 2 units and B has
-- 4. If A wanted to attack again they would only be able to attack
-- with 1 unit (whereas B would still get to defend with 2—clearly
-- this would give B an advantage because the higher of B’s two dice
-- rolls will get matched with A’s single roll.)



-- Exercise 2
-- Given the definitions

-- type Army = Int
-- data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- (which are also included in Risk.hs), write a function with the type
battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
    let att = min 3 $ a - 1 -- count of attackers
        def = max 2 d       -- count of defenders
    dieValues <- replicateM (att + def) die
    let (f, s) = splitAt att dieValues
        nf = reverse $ sort f
        ns = reverse $ sort s
        r = zipWith compare nf ns
        wins = length $ filter (== GT) $ r
        losses = length r - wins
    return $ Battlefield (a - losses) (d - wins)

-- replicateM (a + d) die
-- replicateM d die

-- evalRand

-- fmap (splitAt a) $ replicateM (a + d) die
-- compare



-- which simulates a single battle (as explained above) between two
-- opposing armies. That is, it should simulate randomly rolling the
-- appropriate number of dice, interpreting the results, and updating
-- the two armies to reflect casualties. You may assume that each player
-- will attack or defend with the maximum number of units they are
-- allowed.






-- Exercise 3
-- Of course, usually an attacker does not stop after just a single
-- battle, but attacks repeatedly in an attempt to destroy the entire
-- defending army (and thus take over its territory).  Now implement a
-- function  which simulates an entire invasion attempt, that is,
-- repeated calls to battle until there are no defenders remaining, or
-- fewer than two attackers.

isEnd :: Battlefield -> Bool
isEnd (Battlefield a d) = a < 2 || d < 1

invade :: Battlefield -> Rand StdGen Battlefield
-- invade = (fmap (head . dropWhile (not . isEnd))) . sequence . repeat . battle
invade bf = do
  bf1 <- battle bf
  if isEnd bf1
     then return bf1
     else invade bf1

isWinner :: Battlefield -> Bool
isWinner (Battlefield a _) = a < 2

calcProb :: [Bool] -> Double
calcProb bs = fromIntegral (length $ filter id bs) / (fromIntegral $ toInteger (length bs))

-- Exercise 4
-- Finally, implement a function
successProb :: Battlefield -> Rand StdGen Double
successProb bf = fmap calcProb $ sequence $ fmap (fmap isWinner . invade) $ take 1000 $ repeat bf

-- which runs invade 1000 times, and uses the results to compute a
-- Double between 0 and 1 representing the estimated probability that
-- the attacking army will completely destroy the defending army.
-- For example, if the defending army is destroyed in 300 of the 1000
-- simulations (but the attacking army is reduced to 1 unit in the other
-- 700), successProb should return 0.3.

-- to test, run:
-- let fb = Battlefield 10 8
-- evalRandIO $ successProb fb

-- Exercise 5 (Optional)
-- Write a function
-- exactSuccessProb :: Battlefield -> Double
-- which computes the exact probability of success based on principles
-- of probability, without running any simulations. (This won’t give you
-- any particular practice with Haskell; it’s just a potentially interesting
-- challenge in probability theory.)
