{-# LANGUAGE DeriveFoldable #-}

data BT a = Empty | Node a (BT a) (BT a) deriving (Eq,Ord,Show,Read,Foldable)
singleNodeTree :: a  -> BT a
singleNodeTree x = Node x Empty Empty

insertElement :: Ord a => a -> BT a -> BT a
insertElement x Empty = singleNodeTree x
insertElement x (Node n left right)
 | x < n  = Node n (insertElement x left) (right)
 | x > n  = Node n (left) (insertElement x right)
 | x == n = Node n left right

searchElement :: Ord a => a -> BT a -> Bool
searchElement x Empty = False
searchElement x (Node n left right)
 | x == n = True
 | x > n = searchElement x right
 | otherwise = searchElement x left

buildBinaryTreeFromList :: Ord a => [a] -> BT a
buildBinaryTreeFromList xs = foldr insertElement Empty xs

sortTree :: BT a -> [a]
sortTree Empty = []
sortTree (Node n left right) = sortTree left ++ (n:sortTree right)

sortElementsList :: Ord a => [a] -> [a]
sortElementsList xs = sortTree  $ buildBinaryTreeFromList xs


