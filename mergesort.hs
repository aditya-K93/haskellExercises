mergeSort :: ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort merge xs
 | length xs < 2 = xs
 | otherwise = merge (mergeSort merge firstHalf) (mergeSort merge otherHalf)
  where firstHalf = take ( length xs `div` 2) xs
        otherHalf = drop (length xs `div` 2) xs

merge ::Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge (x1:xs1) (x2:xs2)
 | x1 <= x2 = x1 : merge (xs1) (x2:xs2)
 | otherwise = x2: merge (x1:xs1) (xs2)
