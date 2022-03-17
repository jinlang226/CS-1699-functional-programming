import System.IO
import System.Exit
import System.Random

-- ghc --make wordle.hs  

main :: IO ()
main = do
    content <- readFile "sgb-words.txt"
    target <- getTarget (lines content)
    -- let target = "stews"
    putStrLn target
    putStrLn "Welcome to Wordle by Haskell\n============================\n(4) watts\n   *+---\nYou of 4 more tries.\n'w' is in the word and in the correct spot.\n'a' is in the word but in the wrong spot.\n't' and 's' are not in the word"
    playGame target

getTarget :: [String] -> IO String
getTarget list = randomRIO (0, length list) >>= return . (!!) list

playGame :: [Char] -> IO ()
playGame target = do
    word <- prompt "(6) "
    condition word target
    putStrLn (checkTarget word target) 

    word <- prompt "(5) "
    condition word target
    putStrLn (checkTarget word target) 
    
    word <- prompt "(4) "
    condition word target
    putStrLn (checkTarget word target) 

    word <- prompt "(3) "
    condition word target
    putStrLn (checkTarget word target) 

    word <- prompt "(2) "
    condition word target
    putStrLn (checkTarget word target) 
    
    word <- prompt "(1) "
    if word == target 
        then putStrLn "You Win..." 
        else putStrLn ("\nYou lose... The word is " ++ "\"" ++ target++ "\"" ++ ".")
    replayGame target

condition :: [Char] -> [Char] -> IO ()
condition word target =
    if word == target 
        then restart target
        else return() 

checkTarget :: [Char] -> [Char] -> String
checkTarget word target = 
    compareWordTarget2 (fst wordAndTarget) (snd wordAndTarget) 0 5 (fst wordAndTarget)
    where wordAndTarget = compareWordTarget word target 0 5 ([],target)

compareWordTarget :: [Char] -> String -> Int -> t -> ([Char], String) -> ([Char], String)
compareWordTarget word target index len (res, newTarget) 
    | index >= 5 = (res, newTarget) 
    | index < 5 && word!!index == target!!index = compareWordTarget word changeTarget (index+1) len ((res ++ "*"), changeTarget)
    | index < 5 && myElem (word!!index) target  = compareWordTarget word target (index+1) len ((res ++ [(word!!index)]), target)
    | otherwise = compareWordTarget word target (index+1) len ((res ++ "-"), target)
    where changeTarget = replaceCharAtIndex index '_' target 

compareWordTarget2 :: String -> [Char] -> Int -> t -> String -> String
compareWordTarget2 word newTarget index len res
    | index >= 5 = res
    | index < 5 && myElem (word!!index) newTarget && (word!!index) /= '*' = compareWordTarget2 newRes newTarget (index+1) len newRes 
    | index < 5 && not (myElem (word!!index) newTarget) && (word!!index) /= '*' = compareWordTarget2 newRes newTarget (index+1) len newRes'
    | otherwise = compareWordTarget2 word newTarget (index+1) len res
    where newRes = replaceCharAtIndex index '+' res 
          newRes' = replaceCharAtIndex index '-' res 

-- Usage: replaceCharAtIndex 4 '!' "Hello World!"
replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex index replacement str = strHead ++ [replacement] ++ drop 1 strAfter
  where (strHead, strAfter) = splitAt index str

myElem :: Eq t => t -> [t] -> Bool
myElem _ []       = False
myElem x (y:ys)   = x==y || myElem x ys

replayGame :: [Char] -> IO ()
replayGame target = do
    choice <- prompt "Would you like to play again? (y/n):"
    if choice == "y"
        then playGame target 
        else exitSuccess

restart :: [Char] -> IO ()
restart target = do
    putStrLn "You Win..."
    replayGame target 

prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine 

