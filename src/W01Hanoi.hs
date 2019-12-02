module W01Hanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from to aux = [(from, to)]
hanoi i from to aux = (hanoi (i - 1) from aux to) ++ [(from, to)] ++ (hanoi (i - 1) aux to from)

