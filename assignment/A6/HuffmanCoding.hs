-- module HuffmanCoding
-- (HuffmanCoding(HBit), toHuffmanCode, fromHuffmanCode) where

import BTree 
import Text.XHtml (treeColors)

-- This module defines a new data type called HBit which only consists of two value constructors, 
-- L (for left) and R (for right). 
-- Note that the HBit data type is just an enumerated type. 
-- For simplicity, you are allowed to use automatic deriving for the Show, Eq, and Read type classes. 
-- With the HBit data type, a Huffman coding is simply a list of HBit ([HBit]).
data HBit = L | R deriving (Eq, Read)

instance Show HBit where
    show L = "L"
    show R = "R"


-- ++ [L]


-- data (c, f) where c is a character and f if the frequency of the character c in a given message to be compressed. 
-- A non-leaf node should contain a data ('\NUL', f) where f is the summation of the frequencies of its children. 

-- a Huffman tree will have type BTree (Char, Int).

-- toHuffmanCode :: [Char] -> ([Char], [Char])
-- getFinalTree input = (calcTree $ getList input)!!0

-- fromHuffmanCode :: [Char] -> [Char] -> [Char]


-- To construct a Huffman tree for a message, first we need to start with the frequency of each character in the message, 
-- sort them, and turn them into a list of binary trees. 
freq:: (Ord a) => [a] -> [(a, Int)]
freq []      = []
freq (x: xs) = ins x (freq xs)
   where 
      ins :: (Ord a) => a -> [(a, Int)] -> [(a, Int)]
      ins x ((fk, fv): fs) | x == fk = (fk, fv + 1): fs 
      ins x ((fk, fv): fs) | x <  fk = (x, 1): (fk, fv): fs       
      ins x ((fk, fv): fs) | x >  fk = (fk, fv): ins x fs             
      ins x []                       = (x, 1): []           


sortBySnd [] = []
sortBySnd ((x,a):(y,b):l)
    | l == []   = if a <= b then (x,a):(y,b):[]  else (y,b):(x,a):[] 
    | a >= b    = (y,b) : sortBySnd ((x,a):l)
    | otherwise = (x,a) : sortBySnd ((y,b):l)

getList list  = listToListOfTree $ sortBySnd $ freq list
-- [('1',4),('2',2),('3',3)]


-- freq list to Tree list 
listToListOfTree :: [(a, b)] -> [BTree (a, b)]
listToListOfTree [] = []
listToListOfTree [(x,a)] = [(BTree (x,a) EmptyBTree EmptyBTree)]
listToListOfTree ((x,a) : (y,b):l) = (BTree (x,a) EmptyBTree EmptyBTree) : listToListOfTree ((y,b):l) 


-- 1. Take the least frequent trees, make them the left child and the right child of a new root node that contains the character '\NUL' 
--    and the frequency equal to the sum of frequencies of its children.
-- 2. Remove those two least frequent trees from the list
-- 3. Insert the new tree into the list in the position where the list is still a sorted list of binary trees.
-- 4. Go back to step 1


-- new root freq
newRootValue list = (returnBTreeValue $ list!!0) + (returnBTreeValue $ list!!1)

returnBTreeValue (BTree (c, i) leftBTree rightBTree) = i 

-- list is bstnode list, n is the newRoot freq, return value is new node
makeNewRoot list n = (BTree ('\NUL', n) (list!!0) (list!!1))

dropFirstTwo list = drop 2 list

-- insert new node to whole list
-- [BTree ('x',1) EmptyBTree EmptyBTree,BTree ('y',1insertTreeList (BTree ('\NUL',2) (BTree ('d',1) EmptyBTree EmptyBTree) (BTree ('g',1) EmptyBTree EmptyBTree)) ([BTree ('x',1) EmptyBTree EmptyBTree,BTree ('y',1) EmptyBTree EmptyBTree,BTree ('z',1) EmptyBTree EmptyBTree])) EmptyBTree EmptyBTree,BTree ('z',1) EmptyBTree EmptyBTree]
-- test: insertTreeList (BTree ('\NUL',2) (BTree ('d',1) EmptyBTree EmptyBTree) (BTree ('g',1) EmptyBTree EmptyBTree)) ([BTree ('x',1) EmptyBTree EmptyBTree,BTree ('y',1) EmptyBTree EmptyBTree,BTree ('z',1) EmptyBTree EmptyBTree])
-- insertTreeList EmptyBTree ((BTree (cx, ix) leftBTreex rightBTreex) : xs) = ((BTree (cx, ix) leftBTreex rightBTreex) : xs)
-- insertTreeList (BTree (c, i) leftBTree rightBTree) [] = [(BTree (c, i) leftBTree rightBTree)]
-- insertTreeList (BTree (c, i) leftBTree rightBTree) ((BTree (cx, ix) leftBTreex rightBTreex) : xs) 
--     | i <= ix = (BTree (c, i) leftBTree rightBTree) : ((BTree (cx, ix) leftBTreex rightBTreex) :xs) 
--     | i >  ix = (BTree (cx, ix) leftBTreex rightBTreex) : (insertTreeList (BTree (c, i) leftBTree rightBTree) xs)



-- findRoute nodeValue EmptyBTree res = error "emptyTree"
findRoute :: Char -> BTree (Char, b) -> [HBit] -> (Char, [HBit])
findRoute nodeValue (BTree (c, i) leftBTree rightBTree) res
    | nodeValue == c = (nodeValue, res)
    | nodeValue <  c =  (nodeValue, (res ++ [L]))
    | nodeValue >  c =  (nodeValue, (res ++ [R]))




insertTreeList EmptyBTree ((BTree (cx, ix) leftBTreex rightBTreex) : xs) = ((BTree (cx, ix) leftBTreex rightBTreex) : xs)
insertTreeList (BTree (c, i) leftBTree rightBTree) [] = [(BTree (c, i) leftBTree rightBTree)]
insertTreeList (BTree (c, i) leftBTree rightBTree) ((BTree (cx, ix) leftBTreex rightBTreex) : xs) 
    | i <= ix = (BTree (c, i) leftBTree rightBTree) : ((BTree (cx, ix) leftBTreex rightBTreex) :xs) 
    | i >  ix = (BTree (cx, ix) leftBTreex rightBTreex) : (insertTreeList (BTree (c, i) leftBTree rightBTree) xs)

-- list should be: getList input
calcTree list 
    | length list >= 2 = calcTree (insertTreeList (makeNewRoot list (newRootValue list)) (dropFirstTwo list))
    | otherwise = list
-- list = [BTree ('e',1) EmptyBTree EmptyBTree,BTree ('h',1) EmptyBTree EmptyBTree,BTree ('o',1) EmptyBTree EmptyBTree,BTree ('l',2) EmptyBTree EmptyBTree] 


-- getCode
list = BTree ('\NUL',3) (BTree ('g',1) EmptyBTree EmptyBTree) (BTree ('\NUL',2) (BTree ('d',1) EmptyBTree EmptyBTree) (BTree ('o',1) EmptyBTree EmptyBTree))

