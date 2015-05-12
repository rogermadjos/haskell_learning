import Data.List

-- Excercise 1
skips :: [a] -> [[a]]
skips xs = map (skipper xs) [1..length xs]
  where skipper as n = (fst . unzip . filter (\(_,i) -> mod i n == 0)) (zip as [1..])

-- Excercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) = [b | b > a && b > c] ++ localMaxima (b:c:xs)
localMaxima _ = []

-- Excercise 3
histogram :: [Integer] -> String
histogram xs = intercalate "\n" (map (makeRow histo) (reverse [0..maximum histo - 1])) ++
  "\n==========\n0123456789\n"
  where addItem xs n = zipWith (\x i -> if i == n then x + 1 else x) xs [0..]
        histo = foldl addItem (replicate 10 0) xs
        makeRow xs n = map (\x -> if x > n then '*' else ' ') xs
