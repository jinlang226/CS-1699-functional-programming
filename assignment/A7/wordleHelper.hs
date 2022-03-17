import System.IO

main = do
    putStrLn ("There are "++ show 5757 ++ " words satisfy the following conditions:" )
    hint <- prompt "Enter a word and hints (0 to show remaining words):"
    putStrLn "There are 2 words satisfy the following conditions:"



prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine 

