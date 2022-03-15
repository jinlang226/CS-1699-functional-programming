module HuffmanCoding
(toHuffmanCode, fromHuffmanCode) where

import BTree 

-- This module defines a new data type called HBit which only consists of two value constructors, 
-- L (for left) and R (for right). 
-- Note that the HBit data type is just an enumerated type. 
-- For simplicity, you are allowed to use automatic deriving for the Show, Eq, and Read type classes. 
-- With the HBit data type, a Huffman coding is simply a list of HBit ([HBit]).
data HBit = L | R deriving (Eq, Read)

instance Show HBit where
    show L = "L"
    show R = "R"


-- data (c, f) where c is a character and f if the frequency of the character c in a given message to be compressed. 
-- A non-leaf node should contain a data ('\NUL', f) where f is the summation of the frequencies of its children. 

-- a Huffman tree will have type BTree (Char, Int).
toHuffmanCode :: [Char] -> ([Char], [Char])
toHuffmanCode input = (show tree, show hbitList) 
    where tree = getFinalTree input
          hbitList = getHBitList input (treeToList tree) 0 []


getFinalTree :: [Char] -> BTree (Char, Integer)
getFinalTree input = (calcTree $ getList input)!!0


-- To construct a Huffman tree for a message, first we need to start with the frequency of each character in the message, 
-- sort them, and turn them into a list of binary trees. 
-- 1. Take the least frequent trees, make them the left child and the right child of a new root node that contains the character '\NUL' 
--    and the frequency equal to the sum of frequencies of its children.
-- 2. Remove those two least frequent trees from the list
-- 3. Insert the new tree into the list in the position where the list is still a sorted list of binary trees.
-- 4. Go back to step 1

freq:: (Ord a) => [a] -> [(a, Integer)]
freq []      = []
freq (x: xs) = ins x (freq xs)
   where 
      ins :: (Ord a) => a -> [(a, Integer)] -> [(a, Integer)]
      ins x ((fk, fv): fs) | x == fk = (fk, fv + 1): fs 
      ins x ((fk, fv): fs) | x <  fk = (x, 1): (fk, fv): fs       
      ins x ((fk, fv): fs) | x >  fk = (fk, fv): ins x fs             
      ins x []                       = (x, 1): []           

sortBySnd [] = []
sortBySnd [a] = [a]
sortBySnd ((x,a):xs) = sortBySnd [(c1, i1) | (c1, i1) <- xs, i1 < a] ++ [(x,a)] ++ sortBySnd [(c2, i2) | (c2, i2) <- xs, i2 >= a]

getList list  = listToListOfTree $ sortBySnd $ freq list

-- freq list to Tree list 
listToListOfTree :: [(a, b)] -> [BTree (a, b)]
listToListOfTree [] = []
listToListOfTree [(x,a)] = [(BTree (x,a) EmptyBTree EmptyBTree)]
listToListOfTree ((x,a) : (y,b):l) = (BTree (x,a) EmptyBTree EmptyBTree) : listToListOfTree ((y,b):l) 

-- insert new node to whole list
-- [BTree ('x',1) EmptyBTree EmptyBTree,BTree ('y',1insertTreeList (BTree ('\NUL',2) (BTree ('d',1) EmptyBTree EmptyBTree) (BTree ('g',1) EmptyBTree EmptyBTree)) ([BTree ('x',1) EmptyBTree EmptyBTree,BTree ('y',1) EmptyBTree EmptyBTree,BTree ('z',1) EmptyBTree EmptyBTree])) EmptyBTree EmptyBTree,BTree ('z',1) EmptyBTree EmptyBTree]
-- test: insertTreeList (BTree ('\NUL',2) (BTree ('d',1) EmptyBTree EmptyBTree) (BTree ('g',1) EmptyBTree EmptyBTree)) ([BTree ('x',1) EmptyBTree EmptyBTree,BTree ('y',1) EmptyBTree EmptyBTree,BTree ('z',1) EmptyBTree EmptyBTree])
insertTreeList EmptyBTree ((BTree (cx, ix) leftBTreex rightBTreex) : xs) = ((BTree (cx, ix) leftBTreex rightBTreex) : xs)
insertTreeList (BTree (c, i) leftBTree rightBTree) [] = [(BTree (c, i) leftBTree rightBTree)]
insertTreeList (BTree (c, i) leftBTree rightBTree) ((BTree (cx, ix) leftBTreex rightBTreex) : xs) 
    | i <= ix = (BTree (c, i) leftBTree rightBTree) : ((BTree (cx, ix) leftBTreex rightBTreex) :xs) 
    | i >  ix = (BTree (cx, ix) leftBTreex rightBTreex) : (insertTreeList (BTree (c, i) leftBTree rightBTree) xs)

calcTree list 
    | length list >= 2 = calcTree (insertTreeList (makeNewRoot list (newRootValue list)) (dropFirstTwo list))
    | otherwise = list
    -- list is bstnode list, n is the newRoot freq, return value is new node
    where makeNewRoot list n = (BTree ('\NUL', n) (list!!0) (list!!1)) 
          dropFirstTwo list = drop 2 list
          -- new root freq
          newRootValue list = (returnBTreeValue $ list!!0) + (returnBTreeValue $ list!!1)
              where returnBTreeValue (BTree (c, i) leftBTree rightBTree) = i 

treeToList :: BTree (Char, Integer) -> [(Char, [HBit])]
treeToList (BTree (c, i) leftBTree rightBTree)
    | rightBTree == EmptyBTree && leftBTree == EmptyBTree = [(c, [])] 
    | otherwise = map (addHBit L) (treeToList leftBTree) ++ map (addHBit R) (treeToList rightBTree)
    where addHBit b = \x -> (fst x, b : (snd x))


-- getHBitList input treeList 0 []
getHBitList :: [Char] -> [(Char, [HBit])] -> Int -> [HBit] -> [HBit]
getHBitList input treeList index res
    -- | index < len = map (\p -> if fst p == char then res ++ (snd p) else (getHBitList input treeList (index+1) res)) treeList
    | index < len = getHBitList input treeList (index+1) (res ++ (getHBitListHelper char treeList [] 0 (length(treeList))))
    | otherwise = res
    where len = length(input)
          char = input!!index

-- b = map (\p -> if p == ('b', 0) then ('b', 13) else p) a
getHBitListHelper :: Char -> [(Char, [HBit])] -> [HBit] -> Int -> Int -> [HBit]
getHBitListHelper input treeList res index2 length2 
    | index2 < length2 = if input == (fst $ treeList!!index2) then res ++ snd(treeList!!index2) else getHBitListHelper input treeList res (index2+1) length2 
    | otherwise = res


---------------------------

fromHuffmanCode :: [Char] -> [Char] -> [Char]
fromHuffmanCode t h = fromHuffmanCodeInternal tree hbits
    where tree = read t :: BTree (Char, Int)
          hbits = read h :: [HBit]


fromHuffmanCodeInternal :: BTree (Char, Int) -> [HBit] -> [Char]
fromHuffmanCodeInternal _ [] = ""
fromHuffmanCodeInternal tree hbits =
    symbol:(fromHuffmanCodeInternal tree hbits')
    where
        (symbol, hbits') = consumeSymbol tree hbits


consumeSymbol :: BTree (Char, Int) -> [HBit] -> (Char, [HBit])
consumeSymbol (BTree ('\NUL', _) lTree rTree) (h:hs) =
    consumeSymbol (if h == L then lTree else rTree) hs
consumeSymbol (BTree (c, _) _ _) hs = (c, hs)


-- fromHuffmanCode "BTree ('\\NUL',3) (BTree ('o',1) EmptyBTree EmptyBTree) (BTree ('\\NUL',2) (BTree ('d',1) EmptyBTree EmptyBTree) (BTree ('g',1) EmptyBTree EmptyBTree))" "[R,L,L,R,R]"
