-- quick sort
quickSort [] = []
quickSort [a] = [a]
quickSort (x:xs) = quickSort [a | a <- xs, a < x] ++ [x] ++ quickSort [b | b <- xs, b >= x]

-- merge sort, cut array in half
firstHalf xs = take (div (length xs) 2) xs
secondHalf xs = drop (div (length xs) 2) xs

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergeSort [] = []
mergeSort [a] = [a]
mergeSort x = merge (mergeSort (firstHalf x)) (mergeSort (secondHalf x))

-- insertion sort
insertionSort [] = []
insertionSort [a] = [a]
insertionSort (x:xs) = insert x (insertionSort xs)

insert x [] = [x]
insert x (y:ys) 
    | x < y      =  x:y:ys
    | otherwise  =  y:(insert x ys)

-- selection sort
selectionSort [] = []
selectionSort [a] = [a]
selectionSort xs = x : selectionSort (delete x xs)
                   where x = minimum xs
                   
-- removeItem _ []                 = []
-- removeItem x (y:ys) | x == y    = removeItem x ys
--                     | otherwise = y : removeItem x ys                   

delete x []        = []
delete x (y:ys)    = if x == y then ys 
                     else y : delete x ys    

-- bubbleSort
bubbleSortIter (x:y:xs)
    | x > y             = y : bubbleSortIter (x:xs)
    | otherwise         = x : bubbleSortIter (y:xs)
bubbleSortIter x = x

bubbleSort [] = []
bubbleSort [a] = [a]
bubbleSort s = case bubbleSortIter s of 
               t | t == s    -> t
                 | otherwise -> bubbleSort t
