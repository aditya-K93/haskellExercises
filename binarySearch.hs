-- haskell code for searching an element in a list using Binary Search
-- returns True if elemeent is present else false

binarySearch :: Ord a => [a] -> a -> Bool
binarySearch [] _ = False
binarySearch xs a
 | middle == a = True
 | length xs < 2 = False
 | a > middle = binarySearch secondHalf a
 | otherwise = binarySearch firstHalf a
  where
        firstHalf = take (len `div` 2) xs
        secondHalf = drop (len `div` 2) xs
        middle = head $ drop ((length xs -1) `div` 2) xs
        len = length xs
