mergeSortHelper :: ([a] -> [a] -> [a]) -> [a] -> [a])
mergeSortHelper merge xs
 | length xs < 2 = xs
 | otherwise = merge (mergeSortHelper merge firstHalf) (mergeSortHelper merge otherHalf)
  where firstHalf = take ( length xs `div` 2) xs
        otherHalf = drop (length xs `div` 2) xs

merge ::Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge (x1:xs1) (x2:xs2)
 | x1 <= x2 = x1 : merge (xs1) (x2:xs2)
 | otherwise = x2: merge (x1:xs1) (xs2)

mergeSort :: Ord a => [a] -> [a]
mergeSort xs = mergeSortHelper merge xs
