import Log
import Data.List (elemIndex)

-- Exercise 1
sepWords :: Int -> Char -> String -> [String]
sepWords 0 d xs = [xs]
sepWords n d xs = case elemIndex d xs of
  Just l -> take l xs : sepWords (n-1) d (drop (l+1)  xs)
  Nothing -> [xs]

parseMessage :: String -> LogMessage
parseMessage xs = log $ sepWords 2 ' ' xs
  where toInt s = read s :: Int
        log ["I", a, b] = LogMessage Info (toInt a) b
        log ["W", a, b] = LogMessage Warning (toInt a) b
        log ["E", a, b] = LogMessage (Error (toInt a)) (toInt aa) bb
          where [aa, bb] = sepWords 1 ' ' b
        log _ = Unknown xs

parse :: String -> [LogMessage]
parse xs = map parseMessage (sepWords (maxBound :: Int) '\n' xs)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert new Leaf = Node Leaf new Leaf
insert new@(LogMessage _ nt _) tree@(Node left old@(LogMessage _ ot _) right)
  | nt <= ot = Node (insert new left) old right
  | otherwise = Node left old (insert new right)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log right) = inOrder left ++ [log] ++ inOrder right

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs = map message (filter severe xs)
  where severe (LogMessage (Error e) _ _) = e >= 50
        severe _ = False
        message (LogMessage _ _ m) = m
