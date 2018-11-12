
data BT a = Empty | Node a (BT a) (BT a) deriving (Eq,Ord,Show,Read)

testTree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)

--inOrder traversal (left, Root, Right)
inOrderTraversal :: BT a -> [a]
inOrderTraversal Empty = []
inOrderTraversal (Node n left right) = inOrderTraversal left ++ [n] ++ inOrderTraversal right


--preOrder traversal (root left right)
preOrderTraversal :: BT a -> [a]
preOrderTraversal Empty = []
preOrderTraversal (Node n left right) = [n] ++ preOrderTraversal left ++ inOrderTraversal right

--postOrderTraversal (left, right, root)
postOrderTraversal :: BT a -> [a]
postOrderTraversal Empty = []
postOrderTraversal (Node n left right) = postOrderTraversal left ++ postOrderTraversal right ++ [n]

