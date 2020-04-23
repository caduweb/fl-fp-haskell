
data Tree = Leaf | Node Int Tree Tree deriving Show

isSortedTree :: Tree -> Bool
isSortedTree Leaf = True
isSortedTree (Node _ Leaf Leaf) = True
isSortedTree (Node x Leaf (Node r rl rr)) = 
  and ((x <= r) : map isSortedTree [rl, rr])
isSortedTree (Node x (Node l ll lr) Leaf) = 
  and ((x > l) : map isSortedTree [ll, lr])
isSortedTree (Node x (Node l ll lr) (Node r rl rr)) =
  and ((x > l) : (x <= r) : map isSortedTree [ll, lr, rl, rr])
  
