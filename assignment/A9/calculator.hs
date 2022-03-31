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
    when (not valid) $ putStrLn "not valid"
    let balanced = isBalanced (filterInput input)
    when (not balanced) $ putStrLn "not valid"
    let operatorNum = fst $ isFormal (parse input) 0 0
    let operandNum = snd $ isFormal (parse input) 0 0
    when (operatorNum + 1 > operandNum) $ error "Invalid Expression: Too many operator(s)"
    when (operatorNum + 1 < operandNum) $ error "Invalid Expression: Too many operand(s)"  
    let postfix = infixToPostfix input 
    -- putStrLn postfix
    let result = evaluate postfix 
    putStrLn (input ++ " = " ++ (show result)) 

operatorMapStr :: [String]
operatorMapStr = ["+", "-", "*", "/"] 

isFormal :: [String] -> Int -> Int -> (Int, Int)
isFormal [] operator operand = (operator, operand)
isFormal (x:xs) operator operand
    | x `elem` operatorMapStr = isFormal xs (operator+1) operand
    | isNumber x = isFormal xs operator (operand+1) 
    | otherwise = isFormal xs operator operand 


isNumber :: String -> Bool
isNumber = all isDigit

isDigit :: Char -> Bool
isDigit c =  (fromIntegral (fromEnum c - fromEnum '0') :: Word) <= 9

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