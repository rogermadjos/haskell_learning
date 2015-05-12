import Data.List ((\\))

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/=1) .
  iterate (\x -> if even x then div x 2 else 3 * x + 1)

-- Exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node n l v r)
  | mdl <= mdr = let nl = insert x l in Node (height nl + 1) nl v r
  | otherwise = let nr = insert x r in Node (height nr + 1) l v nr
  where mdl = mDepth l
        mdr = mDepth r

height :: Tree a -> Integer
height Leaf = -1
height (Node n _ _ _) = n

mDepth :: Tree a -> Integer
mDepth Leaf = -1
mDepth (Node _ l _ r) = min (mDepth l) (mDepth r) + 1

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr1 (/=)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x:xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (map f3 . (f4 \\) . map f2. filter f1 . cartProd f4) f4
  where f1 (i,j) = 1 <= i && i <= j && f2 (i,j) <= n
        f2 (i,j) = i + j + 2 * i * j
        f3 x = x * 2 + 1
        f4 = [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
