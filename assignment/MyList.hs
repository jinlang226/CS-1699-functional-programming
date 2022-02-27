-- you are allowed to use the empty list ([]) and the cons (:). 
-- However, you are not allowed to use the concatenation (++) and !! operators and the null function

-- myNull
myNull                    :: [a] -> Bool
myNull []                 =  True
myNull (_:_)              =  False

-- (+++)
(+++) lst1 lst2 = if myNull lst1
                  then lst2
                  else head lst1 : ((tail lst1) +++ lst2)
 
-- myHead
myHead                    :: [a] -> a
myHead (x:_)              =  x
myHead []                 = error "myHead error" 

-- myTail
myTail                    :: [a] -> [a]
myTail [_]                =  []
myTail (_:xs)             =  xs
myTail []                 =  error "myTail error: the list is empty"

-- myReverse
myReverse lst = if myNull lst
                then []
                else myReverse (tail lst) +++ [head lst]

-- myLast
myLast                    :: [a] -> a
myLast [x]                =  x
myLast (_:xs)             =  myLast xs
myLast []                 =  error "myLast error: the list is empty"

-- myInit
myInit [x]                =  []
myInit (x:xs)             =  x : (myInit xs)
myInit []                 =  error "init error: the list is empty"

-- myLength
myLength                  :: [a] -> Int
myLength xs               = lenAcc xs 0

lenAcc []     n = n
lenAcc (_:ys) n = lenAcc ys (n+1)

-- myTake
myTake n xs | 0 < n     = takeHelper n xs
            | otherwise = []

takeHelper _   []     = []
takeHelper 1   (x: _) = [x]
takeHelper m   (x:xs) = x : takeHelper (m - 1) xs

-- myDrop
myDrop n xs     | n <= 0 =  xs
myDrop _ []              =  []
myDrop 1 (_:xs)          =  xs
myDrop n (_:xs)          =  myDrop (n-1) xs

-- maxmin
maxmin lst = if null (tail lst)
                       then (head lst, head lst)
                       else ( if head lst > fst (maxmin (tail lst))
                              then head lst
                              else fst (maxmin (tail lst))
                            ,
                              if head lst < snd (maxmin (tail lst))
                              then head lst
                              else snd (maxmin (tail lst))
                              )

-- myMaximum
myMaximum lst = fst (maxmin lst)

-- myMinimum
myMinimum lst = snd (maxmin lst)

-- mySum
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- myProduct
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

-- myElem
myElem _ []       = False
myElem x (y:ys)   = x==y || myElem x ys