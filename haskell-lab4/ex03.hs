data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

--
data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

--
data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

--1
depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBT :: BinTree a -> [a]  -- preorder
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = [n] ++ flattenBT lt ++ flattenBT rt

flattenBT' :: BinTree a -> [a]  -- inorder
flattenBT' EmptyBT = []
flattenBT' (NodeBT n lt rt) =  flattenBT lt ++ [n] ++ flattenBT rt

flattenBT'' :: BinTree a -> [a]  -- postorder
flattenBT'' EmptyBT = []
flattenBT'' (NodeBT n lt rt) =  flattenBT lt  ++ flattenBT rt ++ [n]

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
insert val EmptyBT = NodeBT val EmptyBT EmptyBT
insert val (NodeBT n lr rt) = NodeBT n (insert val lr) (insert val rt)

insert' :: Ord a => a -> BinTree a -> BinTree a -- insert element into BST
insert' val EmptyBT = NodeBT val EmptyBT EmptyBT
insert' val (NodeBT n lt rt) | val < n = NodeBT n (insert' val lt) rt
                            | val > n = NodeBT n lt (insert' val rt)
                            | otherwise = NodeBT n lt rt

list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)