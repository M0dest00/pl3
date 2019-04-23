quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort pre ++ [x] ++ quicksort post
    where
        pre  = filter (< x) xs
        post = filter (>= x) xs

fibl::[Int]
fibl =  0 : 1 : zipWith (+) fibl (tail fibs)

fibs::[Int]
fibs =  1: 2 : zipWith (+) fibs (tail fibs)

count_stairs:: Int->Int
count_stairs 0 =0
count_stairs n= last (take n fibs)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mergesort ::[Int]->[Int]
mergesort [] = []
mergesort [a] = [a]
mergesort a =
merge (mergesort firsthalf) (mergesort secondhalf)
    where firsthalf = take ((length a) `div` 2) a
          secondhalf = drop ((length a) `div` 2) a

-- timesort::
insert:: Int->[Int]->[Int]
insert x [] =[x]
insert x (y:ys)
    |x<y = x:y:ys
    |otherwise = y:insert x ys

insertionsort::[Int]->[Int]
insertionsort [x]=[x]
insertionsort (x:xs)= insert x (insertionsort xs)

timesort :: [Int]->[Int]
timesort [] = []
timesort [a] = [a]
timesort a =
  merge (insertionsort firsthalf) (insertionsort secondhalf)
    where firsthalf = take ((length a) `div` 2) a
          secondhalf = drop ((length a) `div` 2) a

merge :: [Int] -> [Int] -> [Int]
merge a [] = a
merge [] b = b
merge (a:as) (b:bs)
  | a < b     = a:(merge as (b:bs))
  | otherwise = b:(merge (a:as) bs)
