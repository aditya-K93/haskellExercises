-- haskell program to count maximum occuring element in list along with count
-- eg. maxCharCount "asaddddd" == Just ('d',5) 
-- eg. maxCharCount '' == Nothing

import qualified Data.Map as Map
import Data.List

prepareChar :: [a] -> [(a,Int)]
prepareChar [] = []
prepareChar (x:xs) = (x,1): prepareChar xs

countMap :: Ord k => [k] -> Map.Map k Int
countMap xs = Map.fromListWith (+) $ prepareChar xs

listCharCount :: Ord k => [k] -> [(k, Int)]
listCharCount xs = Map.toList $ countMap xs

maxCustom :: [(a, Int)] -> (a, Int)
maxCustom = maximumBy (\(_,a) (_,b) -> compare a b)

maxCharCountTuple :: Ord a => [a] -> Maybe (a, Int)
maxCharCountTuple [] = Nothing
maxCharCountTuple xs = Just (maxCustom $ listCharCount xs)


-- cleaner Solution using same logic from above
maxCharCount :: Ord a => [a] -> Maybe (a, Int)
maxCharCount [] = Nothing
maxCharCount xs = Just (maxCustom $ Map.toList $ Map.fromListWith (+) [(x,1) | x <- xs])
