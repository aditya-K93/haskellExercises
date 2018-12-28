-- haskell code to find all pairs of integers in a list of integers whose sum is k

module SumPairs (sumPair)
  where

import qualified Data.Set as Set


sumPair :: (Num a, Ord a) => [a] -> a -> Maybe [(a,a)]
sumPair xs n = sumPairHelper xs n [] Set.empty

sumPairHelper :: (Num a ,Ord a) => [a] -> a -> [(a,a)] -> Set.Set a -> Maybe [(a,a)]
sumPairHelper [] _ [] _ = Nothing
sumPairHelper [] _ acc _ = Just acc
sumPairHelper (x:xs) n acc compSet =
  if Set.member x compSet then sumPairHelper xs n (acc ++ [(x, n - x)]) compSet
    else sumPairHelper xs n  acc (Set.insert (n - x) compSet)
