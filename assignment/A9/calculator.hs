import System.IO
import Control.Monad
import AlgExp

-- Enter an algebraic expression
-- (12 * 3)/4+[5 * (6 - 8)]
-- (12 * 3)/4+[5 * (6 - 8)] = -1
-- enter q to exit

-- ghc --make calculator.hs 

main = do
    input <- prompt "Enter an algebraic expression\n"  
    if input == "q" 
        then return() 
        else calc input
    start input
    
start input = do
    unless (input == "q") $ do
        input <- prompt "Enter an algebraic expression\n" 
        if input == "q" 
            then return() 
            else calc input
        start input 

calc input = do
    let valid = isValid input
    let balanced = isBalanced (filterInput input)
    let intfixToPostfix = infixToPostfix input 
    putStrLn input


filterInput :: String -> [Char]
filterInput [] = []
filterInput (x:xs) 
    | x `myElem` allP = x : filterInput xs 
    | otherwise = filterInput xs 


allP = ['(', ')', '[', ']', '{', '}']


prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine 

myElem :: Eq t => t -> [t] -> Bool
myElem _ []       = False
myElem x (y:ys)   = x==y || myElem x ys