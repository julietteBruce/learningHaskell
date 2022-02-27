{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-
removeTrailingSpace :: String -> String
removeTrailingSpace "" = ""
removeTrailingSpace (e:' ':z) = [e]
removeTrailingSpace (x:y) = [x]++(removeTrailingSpace y)
-}

stringToInt :: String -> Int
stringToInt x = (read x + 0)

parseMessageType :: String -> MessageType
parseMessageType x
     | (head w) == "E" = (Error (stringToInt (w!!1)))
     | (head w) == "I" = Info
     | (head w) == "W" = Warning
     where w = words x

parseTime :: String -> Int
parseTime x
     | (head w) == "E" = (stringToInt (w!!2))
     | (head w) == "I" = (stringToInt (w!!1))
     | (head w) == "W" = (stringToInt (w!!1))
     where w = words x

parseComment :: String -> String
parseComment x
     | (head w) == "E" = unwords (drop 3 w)
     | (head w) == "I" = unwords (drop 2 w)
     | (head w) == "W" = unwords (drop 2 w)
     where w = words x

parseMessage :: String -> LogMessage
parseMessage x
     | (head w) == "E" = LogMessage (parseMessageType x) (parseTime x) (parseComment x)
     | (head w) == "I" = LogMessage (parseMessageType x) (parseTime x) (parseComment x)
     | (head w) == "W" = LogMessage (parseMessageType x) (parseTime x) (parseComment x)
     | otherwise = Unknown (unwords w)
     where w = words x


parse :: String -> [LogMessage]
parse x = [(parseMessage y) | y <- (lines x)]

getTime :: LogMessage -> Int
getTime (LogMessage _ t _ ) = t

insert :: LogMessage -> MessageTree -> MessageTree
insert x Leaf = Node Leaf x Leaf
insert x (Node left a right)
     | (getTime x) == (getTime a) = Node left x right
     | (getTime x) < (getTime a) = Node (insert x left) a right
     | (getTime x) > (getTime a) = Node left a (insert x right)

build :: [LogMessage] -> MessageTree
build x = foldr insert Leaf x

{-build [] = Leaf
build [x] = insert x Leaf
build (x:y) = (insert x (build y))-}

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left a right) = (inOrder left)++[a]++(inOrder right)
 


extractType :: LogMessage -> MessageType
extractType (LogMessage e _ _ ) = e

extractError :: MessageType -> Int
extractError (Error a) = a

{-extractErrorLevel :: LogMessage -> [Int]
extractErrorLevel m
     | e == (Error _) =  [(extractError e)]
     | otherwise = []
     where e = extractType m

 data MessageTree = Leaf
                  | Node MessageTree LogMessage MessageTree

parseTime :: String -> String
parseTime (e:m) = t
parseTime (i:m) = (removeTrailingSpace (tail m))
parseTime (w:m) = (removeTrailingSpace (tail m))

parseMessage :: String -> String
parseMessage (e:m) = m
parseMessage (i:m) = m
parseMessage (w:m) = m
-}