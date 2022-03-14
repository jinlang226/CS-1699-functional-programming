module BTree

(BTree (EmptyBTree, BTree), getRootData, getHeight, getNumberOfNodes, showBTree) where 

data BTree a = EmptyBTree
           | BTree a (BTree a) (BTree a)
           deriving (Show, Eq, Read)

getRootData :: BTree a -> a
getRootData EmptyBTree = error "empty tree"
getRootData (BTree r leftBTree rightBTree) = r

getHeight :: BTree a -> Int
getHeight EmptyBTree = 0
getHeight (BTree x leftBTree rightBTree) = 
          1 + max (getHeight leftBTree) (getHeight rightBTree)

getNumberOfNodes :: BTree a -> Int
getNumberOfNodes EmptyBTree = 0
getNumberOfNodes (BTree x leftBTree rightBTree) =
                1 + getNumberOfNodes leftBTree + getNumberOfNodes rightBTree 

-- showBTree function, it must return a string representation of a binary tree which can be pretty printed on the console screen
showBTree :: (Show a, Eq a) => BTree a -> String
showBTree EmptyBTree = "empty root"
showBTree (BTree r leftBTree rightBTree) = drop 2 $ unlines (showBTreeHelper (BTree r leftBTree rightBTree))


showBTreeHelper :: (Show a, Eq a) => BTree a -> [[Char]]
showBTreeHelper (BTree r leftBTree rightBTree) = ("--" ++ show r) : (printSubTree leftBTree rightBTree)
    where printSubTree leftBTree rightBTree = 
            if (rightBTree == EmptyBTree && leftBTree == EmptyBTree) then [] else 
            ((pad "+" "|  ") (showBTreeHelper rightBTree)) ++ ((pad "+" "   ") (showBTreeHelper leftBTree))
            where pad first rest = zipWith (++) (first : repeat rest)

showBTreeHelper EmptyBTree = [""]

-- test
-- aTree = BTree 'a' (BTree 'b' (BTree 'c' EmptyBTree EmptyBTree) (BTree 'd'(BTree 'e' EmptyBTree EmptyBTree) EmptyBTree)) (BTree 'f' EmptyBTree (BTree 'g' EmptyBTree EmptyBTree))
-- aTree = BTree 1 (BTree 2 (BTree 3 EmptyBTree EmptyBTree) (BTree 4 (BTree 5 EmptyBTree EmptyBTree) EmptyBTree)) (BTree 6 EmptyBTree (BTree 7 EmptyBTree EmptyBTree))