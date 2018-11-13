-- mirror image of a binary tree using recursion in haskell
-- mirrorImage (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)) == Node 1 (Node 3 Empty Empty) (Node 2 Empty Empty)
data BT a = Empty | Node a (BT a) (BT a) deriving (Show,Read)

treeSwap :: BT a -> BT a
treeSwap Empty = Empty
treeSwap (Node n left right) = Node n right left

mirrorImage :: BT a -> BT a
mirrorImage Empty = Empty
mirrorImage (Node n left right) = treeSwap (Node n (mirrorImage left) (mirrorImage right))

