import System.IO

-- ghc --make wordle.hs  

main = do
    putStrLn "Welcome to Wordle by Haskell\n============================\n(4) watts\n   *+---\nYou of 4 more tries.\n'w' is in the word and in the correct spot.\n'a' is in the word but in the wrong spot.\n't' and 's' are not in the word"
    word <- prompt "(6) "
    putStrLn(word)

prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine 