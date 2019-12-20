{-# OPTIONS_GHC -Wall #-}
module W02LogAnalysis where

import W02Log

parseMessage :: String -> LogMessage
parseMessage s =
  case t of
    "E" -> LogMessage (Error l) (read $ head $ drop 2 msgs :: Int) (unwords $ drop 3 msgs)
    "W" -> LogMessage Warning l (unwords $ drop 2 msgs)
    "I" -> LogMessage Info l (unwords $ drop 2 msgs)
    _ -> Unknown s
  where msgs = words s
        t = head msgs
        l = read $ head $ tail msgs :: Int
        
parse :: String -> [LogMessage]
parse s =  map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node lt r@(LogMessage _ tsr _) rt)
  | ts <= tsr = Node (insert m lt) r rt
  | ts > tsr = Node lt r (insert m rt)


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt m rt) = (inOrder lt) ++ [m] ++ (inOrder rt)


isCriticalError :: LogMessage -> Bool
isCriticalError (LogMessage (Error l) _ _) = l >= 50
isCriticalError _ = False

toString :: LogMessage -> String
toString (LogMessage _ _ s) = s
toString (Unknown s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toString . filter isCriticalError

-- testParse parse 10 "./error.log"
-- testWhatWentWrong parse whatWentWrong "error.log"
