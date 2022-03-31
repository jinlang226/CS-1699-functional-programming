module AlgExp
(isValid, isBalanced, infixToPostfix, evaluate, parse) where  

-- ghc --make AlgExp.hs

-- evaluate a string representation of an algebraic expression into a number

-- This function checks whether a given string contains only valid characters. 
-- In an algebraic expression (in our case), can only contain characters 
-- '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', '*', '/', '(', ')', '[', ']', '{', '}', ' '
isValid :: [Char] -> Bool
isValid [] = True
isValid (x:xs) 
    | x `elem` validChar = isValid xs
    | otherwise = error "The expression contains an invalid symbol."  


-- This function checks whether a given string representation of an algebraic expression is balanced
-- based on delimiters '(', ')', '[', ']', '{', and '}'. 
-- Note that if the given string is not valid (contains characters
-- that are not allowed), isBalanced should return False.
isBalanced :: [Char] -> Bool
isBalanced xs = isBalanced' xs []

isBalanced' [] [] = True
isBalanced' [] _  = error "The expression is not balanced." 

isBalanced' ('(':xs) ys = isBalanced' xs (')':ys)
isBalanced' ('[':xs) ys = isBalanced' xs (']':ys)
isBalanced' ('{':xs) ys = isBalanced' xs ('}':ys)

isBalanced' _  [] = error "The expression is not balanced." 
isBalanced' (x:xs) (y:ys) = (x == y) && (isBalanced' xs ys)


validChar = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', '*', '/', '(', ')', '[', ']', '{', '}', ' ']
allP = ['(', ')', '[', ']', '{', '}']
leftP = ['(', '[', '{']
numMap = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']
notNumMap = ['(', ')', '[', ']', '{', '}', '+', '-', '*', '/']
notNumMapStr :: [String]
notNumMapStr = ["(", ")", "[", "]", "{", "}", "+", "-", "*", "/"]
operatorMapStr :: [String]
operatorMapStr = ["+", "-", "*", "/"] 
openP :: [String]
openP = ["(", "[", "{"]
closeP :: [String]
closeP = [ ")", "]", "}"]

-- Turn the infix form of an algebraic expression into the postfix form if the expression is valid and balanced. For example:
-- infixToPostfix "(12 + 5) * 3" should return the string "12 5 + 3 *"
-- infixToPostfix "12 + 5 * 3" should return the string "12 5 3 * +"
infixToPostfix :: [Char] -> [Char]
infixToPostfix input = concat $ infixToPostfix' (parse input) [] []

--                  input,     stack,        res                 (stack is reversed in order)
infixToPostfix' :: [String] -> [String] -> [String] -> [String]
infixToPostfix' [] [] res = res
infixToPostfix' [] stack res = infixToPostfix' [] (tail stack) (res ++ [head stack] ++ [" "])
infixToPostfix' (x:xs) stack res
    | x `elem` openP = infixToPostfix' xs (x:stack) res -- Push ( onto the stack.
    -- Pop operators from the stack and append them to the output expression until an open parenthesis is popped. 
    -- Discard both parentheses.
    | x `elem` closeP = infixToPostfix' xs s r 
    -- Pop operators from the stack, appending them to the output expression, 
    -- until the stack is empty or its top entry has a lower precedence than the new operator. 
    -- Then push the new operator onto the stack.
    | x `elem` operatorMapStr = infixToPostfix' xs s1 r1 
    | otherwise = infixToPostfix' xs stack (res ++ [x] ++ [" "])
    where s = dropWhileMy openP stack
          r = res ++ takeWhileMy openP stack
          s1 = x : dropWhileMy2 x stack
          r1 = res ++ takeWhileMy2 x stack
        --   cond o2 = o2 `elem` operatorMapStr && compareOperator x < compareOperator o2
        --   spl = span cond stack
        --   s1 = x : snd spl
        --   r1 = res ++ fst spl ++ [" "]

dropWhileMy2 _ [] = []
dropWhileMy2 y (x:xs)
    | compareOperator y <= compareOperator x = dropWhileMy2 y xs
    | otherwise = x:xs

takeWhileMy2 _ [] = []
takeWhileMy2 y (x:xs) 
    | compareOperator y <= compareOperator x =  [x]  ++ [" "] ++ takeWhileMy2 y xs
    | otherwise = []

dropWhileMy _ [] = []
dropWhileMy p (x:xs)
    | x `elem` p = xs
    | otherwise = dropWhileMy p xs

takeWhileMy _ [] = []
takeWhileMy p (x:xs) 
    | x `elem` p =  [] 
    | otherwise  =  [x] ++ [" "] ++ takeWhileMy p xs

compareOperator x
    | x == "-" = 2
    | x == "+" = 2
    | x == "/" = 3
    | x == "*" = 3
    |otherwise = 1

parse :: String -> [String]
parse [] = []
parse (x:xs)
    | x == ' ' = parse xs
    | x `elem` notNumMap = [x] : parse xs
    | otherwise = fst parsedNumber : parse (snd parsedNumber)
    where parsedNumber = parseNumber (x:xs)


parseNumber :: String -> (String, String)
parseNumber[] = ([],[])
parseNumber (x:xs)
    | x `elem` numMap = (x : fst parsed, snd parsed)
    | otherwise = ([], x:xs)
    where parsed = parseNumber xs


-- If the given string is a valid and balanced algebraic expression, the evaluate function will return the result. For simplicity, you should use the div function for division. Note that the Integral type class is a type constraint because we use the div function. Similarly, we have to use the read function to turn a string representation of an integer into an integer.
evaluate :: (Integral a, Read a) => [Char] -> a
evaluate xs = head (foldl calculation [] (words xs)) 
    where calculation (x:y:xs) "+" = (x+y):xs
          calculation (x:y:xs) "-" = (x-y):xs
          calculation (x:y:xs) "*" = (x*y):xs
          calculation (x:y:xs) "/" = (x `div` y):xs
          calculation xs y = read y:xs
