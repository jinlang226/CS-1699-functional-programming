 
 
 
 
 main = do
    putStrLn "Welcome to Wordle by Haskell\n============================\n(4) watts\n   *+---\nYou of 4 more tries.\n'w' is in the word and in the correct spot.\n'a' is in the word but in the wrong spot.\n't' and 's' are not in the word"
    choice <- prompt "Enter 1 to Encode or 2 to Decode: "
    key <- prompt "Enter a key (integer): "
    msg <- prompt "Enter a message: "
    let result = cipher choice key msg
    putStrLn ("Result: " ++ result)

 prompt :: String -> IO String
 prompt str = do
    putStr str
    hFlush stdout
    getLine

 cipher choice key msg = if (read choice :: Int) == 1
                         then encode (read key :: Int) msg
                         else decode (read key :: Int) msg
 encode :: Int -> [Char] -> [Char]
 encode n xs = map (chr . (+n) . ord) xs
 decode :: Int -> [Char] -> [Char]
 decode n xs = encode (negate n) xs