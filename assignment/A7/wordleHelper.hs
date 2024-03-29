import System.IO
import Control.Monad

-- ghc --make wordleHelper.hs 

-- "aeiou**---"
-- containAtPosition "aexxx**--*" 0 '*' 5 [('a', 0), ('e', 1)]

main = do
    content <- readFile "sgb-words.txt"
    let list = lines content
    putStrLn ("There are "++ show 5757 ++ " words satisfy the following conditions: ")
    input <- prompt "Enter a word and hints (0 to show remaining words): "
    let resNotContainAt = containAtPosition input 0 '+' 5 []
    let resContainAt = containAtPosition input 0 '*' 5 []
    let resNotContain' = containAtPosition input 0 '-' 5 []
    let resNotContain = [(char, 0) | (char, _) <- resNotContain', char `notElem` [fst req | req <- resNotContainAt ++ resContainAt]]
    let resultList = getFilterContainNotAt resNotContainAt (getFilterNotContain resNotContain (getFilterContainAt resContainAt list))
    when (input == "0") $ resultZero input resContainAt resNotContain resNotContainAt list
    printRequirement input resContainAt resNotContain resNotContainAt resultList
    restartHelper resContainAt resNotContain resNotContainAt resultList


restartHelper resContainAt resNotContain resNotContainAt resultList = do
    let l = "not q"
    unless (l == "q") $ do
        input <- prompt "Enter a word and hints (0 to show remaining words): " 
        let newResContainAt = containAtPosition input 0 '*' 5 resContainAt 
        let newResNotContainAt = containAtPosition input 0 '+' 5 resNotContainAt
        let newResNotContain' = containAtPosition input 0 '-' 5 resNotContain 
        let newResNotContain = [(char, 0) | (char, _) <- newResNotContain', char `notElem` [fst req | req <- newResNotContainAt ++ newResContainAt]]
        let newResultList = getFilterContainNotAt newResNotContainAt (getFilterNotContain newResNotContain (getFilterContainAt newResContainAt resultList))
        if input == "0"
            then resultZero input resContainAt resNotContain resNotContainAt resultList
            else printRequirement input newResContainAt newResNotContain newResNotContainAt newResultList
        restartHelper newResContainAt newResNotContain newResNotContainAt newResultList-- recursive step here


resultZero input resContainAt resNotContain resNotContainAt resultList = do
    putStrLn $ show resultList
    printRequirement input resContainAt resNotContain resNotContainAt resultList
    
-- filter res list 
-- [('a', 0), ('b', 1)]

-- 只用 list comprehension
-- getFilterContainAt reqs words = [word | word <- words, all (==True) [word!!idx == char | (char, idx) <- reqs]]

-- 用 filter 简化一下，但可能反而没那么直观了
getFilterContainAt reqs words = filter check words
    where check word = all (==True) [word!!idx == char | (char, idx) <- reqs]

getFilterNotContain reqs words = filter check words
    where check word = all (==True) [not $ char `myElem` word | (char, _) <- reqs]

getFilterContainNotAt reqs words = filter check words
    where check word = all (==True) [word!!idx /= char && char `myElem` word | (char, idx) <- reqs]

-- do not contain xxx
filterNotContain _ [] = []
filterNotContain (wordIndex, index) (x : xs) -- target list
    | not (myElem wordIndex x) = x : filterNotContain (wordIndex, index) xs
    | otherwise = filterNotContain (wordIndex, index) xs 

-- contain at position
filterContainAt (_, _) [] = []
filterContainAt (wordIndex, index) (x : xs) -- target list
    | x!!index == wordIndex = x : filterContainAt (wordIndex, index) xs
    | otherwise = filterContainAt (wordIndex, index) xs 

filterContainNotAt (_, _) [] = []
filterContainNotAt (wordIndex, index) (x : xs)
    | x!!index /= wordIndex && myElem wordIndex x = x : filterContainNotAt (wordIndex, index) xs
    | otherwise = filterContainNotAt (wordIndex, index) xs 

printRequirement input resContainAt resNotContain resNotContainAt resultList= do
    putStrLn ("There are " ++ show (length resultList) ++ " words satisfy the following conditions:") --
    putStr " - Do not Contain "
    -- 1. Do not contain eodt
    putStrLn (printDoNotContain resNotContain)
    -- 2. Contain 'i' but not at positions [2]
    putStr (printDoNotContainAtPosition resNotContainAt)
    -- 3. Contain 'i' at position 3
    putStr (printContainAtPosition resContainAt)
        
printDoNotContainAtPosition  [] = []
printDoNotContainAtPosition ((character, index) : xs) = " - Contain '" ++ [character] ++ "' but not at position [" ++ show index ++ "]\n" ++ printDoNotContainAtPosition xs 

printDoNotContain [] = [] 
printDoNotContain ((character, index) : xs) = character : printDoNotContain xs

printContainAtPosition [] = [] 
printContainAtPosition ((character, index) : xs) = " - Contain '" ++ [character] ++ "' at position " ++ show index ++ "\n" ++ printContainAtPosition xs 

      
-- Contain 'a' at position 0 
-- usage: containAtPosition "aeiou**---" 0 '*' 5 []
containAtPosition1 :: [Char] -> Int -> Char -> Int -> [(Char,Int)]
containAtPosition1 input index operation len 
    | index < len && hintIndex /= operation = containAtPosition1 input (index + 1)  operation len  
    | index < len && hintIndex == operation = (wordIndex, index) : containAtPosition1 input (index + 1) operation len  
    | otherwise = [] 
    where hint = getHint input
          word = getWord input
          hintIndex = hint!!index
          wordIndex = word!!index

-- usage: containAtPosition "aeiou**---" 0 '*' 5 [(' ', 0)]
-- containAtPosition "aeiou*****" 0 '*' 5 [('a', 0), ('e', 1)]    
containAtPosition :: [Char] -> Int -> Char -> Int -> [(Char, Int)] -> [(Char, Int)]
containAtPosition input index operation len x 
    | input == "0" = x
    | index < len && hintIndex /= operation = containAtPosition input (index + 1)  operation len x 
    | index < len && hintIndex == operation && not (myElem (wordIndex, index) x)= (wordIndex, index) : containAtPosition input (index + 1) operation len x 
    | index < len && hintIndex == operation && myElem (wordIndex, index) x= containAtPosition input (index + 1) operation len x 
    | otherwise = x
    where hint = getHint input
          word = getWord input
          hintIndex = hint!!index
          wordIndex = word!!index

-- usage splitByIndices "abcde12345"  [5]
splitByIndices :: [Char] -> [Int] -> [[Char]]
splitByIndices input indexlist = foldr (\elem acc -> [fst $ splitAt elem $ head acc] ++ [snd $ splitAt elem $ head acc] ++ (tail acc)) [input] indexlist

prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine 

myElem :: Eq t => t -> [t] -> Bool
myElem _ []       = False
myElem x (y:ys)   = x==y || myElem x ys

getWord input = (splitByIndices input [5] ) !! 0
getHint input = (splitByIndices input [5] ) !! 1