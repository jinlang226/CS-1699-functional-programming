import System.IO
import Control.Monad

main = do
    putStrLn ("There are "++ show 5757 ++ " words satisfy the following conditions:")
    restartHelper 


restartHelper :: IO ()
restartHelper = do
    let l = "not q" 
    content <- readFile "sgb-words.txt"
    let list = lines content
    -- unless will execute its block if the condition is False
    unless (l == "q") $ do
        input <- prompt "Enter a word and hints (0 to show remaining words):"

        putStrLn ("There are " ++ show __ ++ " words satisfy the following conditions:")
        
        Do not contain eodt
        Contain 'i' but not at positions [2]
        Contain 'u' but not at positions [4]
        Contain 'i' at position 3
        Contain 'u' at position 1
        Contain 'a' at position 0

        input2 <- prompt "Enter a word and hints (0 to show remaining words):" 
        if input2 == "0"
            then -- show result
            else restartHelper 

        restartHelper  -- recursive step here

getWord input = (splitByIndices input [5] ) !! 0
getHint input = (splitByIndices input [5] ) !! 1

      
-- Contain 'a' at position 0 
-- usage: containAtPosition "aeiou**---" 0 '*' 5 
containAtPosition :: [Char] -> Int -> Char -> Int -> [(Char,Int)]
containAtPosition input index operation len 
    | index < len && hintIndex /= operation = containAtPosition input (index + 1)  operation len 
    | index < len && hintIndex == operation = (wordIndex, index) : containAtPosition input (index + 1) operation len
    | otherwise = []
    where hint = getHint input
          word = getWord input
          hintIndex = hint!!index
          wordIndex = word!!index

-- do not contain xxx
filterNotContain (_, _) [] = []
filterNotContain (wordIndex, index) (x : xs) -- target list
    | x!!index /= wordIndex = x : filterNotContain (wordIndex, index) xs
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

