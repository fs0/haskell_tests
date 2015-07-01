quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = s ++ [x] ++ b
    where
        s = quicksort [a | a <- xs, a <= x]
        b = quicksort [a | a <- xs, a > x]
