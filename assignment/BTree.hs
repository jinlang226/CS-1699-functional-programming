import Distribution.Simple.Utils (xargs)
data BTree = EmptyBTree
           | BTree Int BTree BTree
           deriving (Show, Eq)

bstAdd :: Int -> BTree -> BTree
bstAdd t EmptyBTree = BTree t EmptyBTree EmptyBTree
bstAdd t (BTree r leftBTree rightBTree) 
    | t == r = BTree r leftBTree rightBTree
    | t < r  = BTree r (bstAdd t leftBTree) rightBTree 
    | t > r  = BTree r leftBTree (bstAdd t rightBTree) 


bstFromList :: [Int] -> BTree
firstHalf xs = take (div (length xs) 2) xs
secondHalf xs = drop (div (length xs) 2) xs
bstFromList [] = EmptyBTree 
bstFromList (x:xs) = BTree x (bstFromList (firstHalf xs) ) (bstFromList (secondHalf xs))


bstContains :: Int -> BTree -> Bool
bstContains _ EmptyBTree = False 
bstContains t (BTree r leftBTree rightBTree) 
    | t == r = True
    | t < r  = bstContains t leftBTree
    | t > r  = bstContains t rightBTree 


btHeight :: BTree -> Int
btHeight EmptyBTree = 0
btHeight (BTree x leftBTree rightBTree) = 
         1 + max (btHeight leftBTree) (btHeight rightBTree)


btNumberOfNodes :: BTree -> Int
btNumberOfNodes EmptyBTree = 0
btNumberOfNodes (BTree x leftBTree rightBTree) =
                1 + btNumberOfNodes leftBTree + btNumberOfNodes rightBTree 


-- Preorder Traversal
--   - Visit the root, visit all the nodes in he root's left subtree, and
--     visiting all the nodes in the root's right subtree.
btPreorder :: BTree -> [Int]
btPreorder EmptyBTree = []
btPreorder (BTree r leftBTree rightBTree) = [r] ++ btPreorder leftBTree ++ btPreorder rightBTree 


-- Inorder Traversal
--   - Visit all the nodes in the root's left subtree, visit the root, and 
--     visit all the nodes in the root's right subtree.
btInorder :: BTree -> [Int]
btInorder EmptyBTree = []
btInorder (BTree r leftBTree rightBTree) = btInorder leftBTree ++ [r] ++ btInorder rightBTree 


-- Postorder Traversal
--   - Visit all the nodes in the root's left subtee, visit all the nodes in the
--     root's right subtree, and visit the root
btPostorder :: BTree -> [Int]
btPostorder EmptyBTree = []
btPostorder (BTree r leftBTree rightBTree) = btPostorder leftBTree ++ btPostorder rightBTree ++ [r] 


bstMax :: BTree -> Int
minVal :: Int
minVal = minBound :: Int
bstMax EmptyBTree = minVal 
bstMax (BTree r leftBTree rightBTree) = max (bstMax leftBTree) (max r (bstMax rightBTree)) 


bstMin :: BTree -> Int
maxVal :: Int
maxVal = maxBound :: Int
bstMin EmptyBTree = maxVal 
bstMin (BTree r leftBTree rightBTree) = min (bstMin leftBTree) (min r (bstMin rightBTree)) 


bstRemove :: Int -> BTree -> BTree
bstRemove _  EmptyBTree = EmptyBTree
bstRemove x (BTree r leftBTree rightBTree) 
    | x == r = bstRemoveRoot (BTree r leftBTree rightBTree)
    | x < r  = BTree r (bstRemove x leftBTree) rightBTree
    | x > r  = BTree r leftBTree (bstRemove x rightBTree)

bstRemoveRoot :: BTree -> BTree
bstRemoveRoot (BTree r leftBTree EmptyBTree) = leftBTree 
bstRemoveRoot (BTree r EmptyBTree rightBTree) = rightBTree 
bstRemoveRoot (BTree r leftBTree rightBTree) = (BTree newR removedLeftTree rightBTree)
    where newR            = leftMaxElement leftBTree
          removedLeftTree = bstRemove newR leftBTree

         
leftMaxElement :: BTree -> Int
leftMaxElement (BTree r EmptyBTree rightBTree) = r
leftMaxElement (BTree r leftBTree rightBTree) = leftMaxElement leftBTree