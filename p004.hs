-- 左右どちらから読んでも同じ値になる数を回文数という.
-- 2桁の数の積で表される回文数のうち, 最大のものは 9009 = 91 × 99 である.
-- では, 3桁の数の積で表される回文数の最大値を求めよ.

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]

quicksort [] = []
quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gteq
  where
    lt = filter (<x) xs
    gteq = filter (>=x) xs

main = do
  print $ last
        $ quicksort
        $ filter (\x -> x > 90000)
        $ filter (\x -> myreverse ( show x ) == show x)
        $ map (\(x,y) -> x*y) [(x,y) | x <- [1..999], y <- [1..999], x >= y]
