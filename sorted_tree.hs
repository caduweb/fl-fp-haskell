
data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
 1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node x lst rst) = x + treeSum lst + treeSum rst

isSortedTree :: Tree -> Bool
isSortedTree Leaf = True
isSortedTree (Node _ Leaf Leaf) = True
isSortedTree (Node x Leaf (Node r rl rr)) = 
  and ((x <= r) : map isSortedTree [rl, rr])
isSortedTree (Node x (Node l ll lr) Leaf) = 
  and ((x > l) : map isSortedTree [ll, lr])
isSortedTree (Node x (Node l ll lr) (Node r rl rr)) =
  and ((x > l) : (x <= r) : map isSortedTree [ll, lr, rl, rr])

