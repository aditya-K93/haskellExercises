-- a hacky yet neat although partial implementation of quicksort in haskell
-- please not this is not the originally proposed quicksort algorithm as it does not sort in-place.
-- an inplace implementation is rather tedious in haskell due to default immutability with vanilla dtypes
-- do not use in production as this is order of magnitudes slower than correponding C implementation

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort(left) ++ [pivot] ++ quickSort(right)
 where
   left = filter (< pivot) xs
   right = filter (>= pivot) xs
   pivot = x

-- one liner solution
quickSortNeat :: Ord a => [a] -> [a]
quickSortNeat [] =[]
quickSortNeat (x:xs) = quickSortNeat [ a | a <- xs, a < x] ++ [x] ++ quickSortNeat [a | a <- xs, a >= x]
