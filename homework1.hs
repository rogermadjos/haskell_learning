-- Excercise 1
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = (toDigits . div x) 10 ++ [ mod x 10 ]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Excercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse $ zipWith (*) (reverse x) (cycle [1,2])

-- Excercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- Excercise 4
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- Excercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- Excercise 6 (Frame-Stewart algorithm)
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d = hanoiFS n [a,b,c,d]

hanoiFS :: Integer -> [Peg] -> [Move]
hanoiFS 0 _ = []
hanoiFS 1 (a:b:rest) = [(a, b)]
hanoiFS n (a:b:c:rest) =
  hanoiFS k (a:b:c:rest) ++
  hanoiFS (n - k) (a:b:rest) ++
  hanoiFS k (c:b:a:rest)
  where k | null rest   = n - 1
          | otherwise   = div n 2
