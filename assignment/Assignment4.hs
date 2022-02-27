import Language.Haskell.TH.Syntax (Lit(StringL))
-- Assignment 4

-- elemNum y xs should return the number of the item y in the given list xs. If the given list is empty, simply return 0.
elemNum :: Eq a => a -> [a] -> Int
elemNum _ [] = 0
elemNum y (x:xs) 
    | y == x = 1 + elemNum y xs
    | otherwise = elemNum y xs


-- Given a list of integers, maxOccurs returns a pair (item, num) where item is the largest item in the given list and num is the number of occurrences of the largest item in the given list. You can assume that the given list will not be an empty list.
maxOccurs :: Ord a => [a] -> (a, Int)
maxOccurs xs = last ((map (\xs -> (head xs, length xs)) . group . quickSort) xs)
                

group [] = []
group (x:xs) = group_loop [x] x xs
    where
    group_loop acc c [] = [reverse acc]
    group_loop acc c (y:ys) 
        | y == c    = group_loop (y:acc) c ys
        | otherwise = reverse acc : group_loop [y] y ys


quickSort [] = []
quickSort [a] = [a]
quickSort (x:xs) = quickSort [a | a <- xs, a < x] ++ [x] ++ quickSort [b | b <- xs, b >= x]


-- The unique function should return the list of unique elements from the given list. 
-- For example, unique [3,5,5,1,2,3,4] should be [1,2,4]
onlyUnique :: Eq a => [a] -> [a]
onlyUnique [] = []
onlyUnique (x:xs) = x : onlyUnique (filter (/= x) xs)


-- Turn a given list into a new list without duplicates (a set). 
-- For example, listToSet [3,5,5,1,2,3,4] should be [3,5,1,2,4].
listToSet :: Eq a => [a] -> [a]
listToSet []  = []
listToSet [x] = [x]
listToSet (x1:x2:xs)
      | x1 == x2  = listToSet (x1:xs)
      | otherwise = x1 : listToSet (x2:xs)


-- subset xs ys where xs and ys are list representations of sets (no duplicate), should return True if xs is a subset of ys. 
-- Otherwise, it should return False. You can assume that xs has no duplicates and ys has no duplicates.
subset :: Eq a => [a] -> [a] -> Bool
subset a b = null [x | x <- a , elem x b == False]


-- setEq xs ys where xs and ys are list representations of sets (no duplicate), should return True if xs (as a set) is consider equal to ys. 
-- Otherwise, it should return False. You can assume that xs has no duplicates and ys has no duplicates.
setEq :: Eq a => [a] -> [a] -> Bool
setEq xs ys = and (zipWith (==) xs ys)


-- union xs ys where xs and ys are list representations of sets (no duplicate), 
-- should return a list representation of the set xs unions with the set ys. 
-- You can assume that xs has no duplicates and ys has no duplicates.
union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, not (elem y xs)]


-- intersect xs ys where xs and ys are list representations of sets (no duplicate), should return a list representation of the set xs intersects with the set ys. You can assume that xs has no duplicates and ys has no duplicates.
intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = [y | y <- ys, elem y xs]


-- different :: Eq a => [a] -> [a] -> [a]
-- different xs ys where xs and ys are list representations of sets (no duplicate), 
-- should return a list representation of the set xs subtracted by the set ys (set different). 
-- You can assume that xs has no duplicates and ys has no duplicates.
different :: Eq a => [a] -> [a] -> [a]
different xs ys = filter (`notElem` ys) xs   


-- palindrome :: [Char] -> Bool
-- If the given string is a palindrome, return True. Otherwise, return False.
palindrome :: [Char] -> Bool
palindrome w = w == reverse w


-- sumSquare :: Int -> Int
-- sumSquare n returns the value of 12 + 22 + 33 + ... + n2. You can assume that n >= 0.
sumSquare :: Int -> Int
sumSquare n = sum $ map (^2) [1..n] 


-- pythagoreans :: Int -> [(Int,Int,Int)]
-- pythagoreans n returns a list of all triples (x, y, z) where x2 + y2 = z2 where x, y, and z are at most n. For example, pythagoreans 10 should return [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
pythagoreans :: Int -> [(Int,Int,Int)]
pythagoreans n = [(a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b] , a^2 + b^2 == c^2] 

-- uniquePyths :: Int -> [(Int,Int,Int)]
-- uniquePyths n returns a list of triples (x, y, z) from pythagoreans n where x <= y <= z
uniquePyths :: Int -> [(Int,Int,Int)]
uniquePyths n = [(a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b] , a^2 + b^2 == c^2, a <= b,  b <= c]  

-- perfects :: Int -> [Int]
-- perfects n returns the list of all perfect numbers up to and including n. 
-- Note that a positive integer is perfect if it equals to the sum of its factors, 
-- excluding the number itself. For example, 1, 2, and 3 are factors of 6 and 1 + 2 + 3 = 6. 
-- Thus, 6 is a perfect number. For another example, 1, 2, 4, 7, 14 are factors of 28 and 1 + 2 + 4 + 7 + 14 = 28. Thus, 28 is a perfect number.
perfects :: Int -> [Int]
perfects n = [i | i <- [1..n], perfect i]

perfect n = sum (factors n) == n * 2

factors n = [ i | i <- [1..n], mod n i == 0 ]


-- binToInt :: [Char] -> Int
-- binToInt str returns the value of a given string representation of an unsigned binary number str in integer. 
-- For example binToInt "0010110" is 22. Note that str will be a strings of 0s and 1s. 
-- You can assume that there will be no other symbols in a given string other than 0s and 1s. If the given string is empty, simply return 0.
binToInt :: [Char] -> Int
binToInt [] = 0
binToInt (x:xs) = (scanChar x * 2^(length xs)) + (binToInt xs)

scanChar c 
    | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'


-- intToBin n returns the unsigned binary representation (as a string of 0s and 1s) of the given positive number n. For example, intToBin 22 is "10110". You can assume that the given integer will be greater than or equal to 0.
intToBin :: Int -> [Char]
intToBin 0 = []
intToBin 1 = show 1
intToBin n = (intToBin (n `div` 2)) ++ show (n `mod` 2)


-- signedIntToBin :: Int -> [Char]
-- signedIntToBin n returns the binary representation (as a string of 0s and 1s) in 32-bit Two's Complement format of the given number n.
signedIntToBin :: Int -> [Char]
signedIntToBin n | n >= 0 = fitLength binary '0'
                 | n <  0 = fitLength (signedIntHelper (intToBin (0-n))) '1'
                  where binary  = intToBin n

fitLength n x = replicate (32 - length(n)) x ++ n                

-- find the first 1, flip before
signedIntHelper :: [Char] -> [Char]
signedIntHelper n | last n == '1' && length(n) >  1 = flipBits (init n) ++ "1"
                  | last n == '1' && length(n) == 1 = "1"
                  | last n == '0' = signedIntHelper (init n) ++ "0"
                  | otherwise = "error"
                 
flipBits "1" = "0"
flipBits "0" = "1"
flipBits (x:xs)| last (x:xs) == '0' = (flipBits (init (x:xs))) ++ "1"
               | last (x:xs) == '1' = (flipBits (init (x:xs))) ++ "0"

-- toRomanNumeral :: Int -> [Char]
-- toRomanNumeral n returns a string representation of the roman numeral of n. You can assume that 1 <= n <= 3999. For the roman numeral, I is 1, V is 5, X is 10, L is 50, C is 100, D is 500, and M is 1000.
characterMap = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"),
           (90,"XC"), (50,"L"), (40,"XL"), (10,"X"), (9,"IX"), (5,"V"),
           (4,"IV"), (1,"I")]

toRomanNumeral :: Int -> [Char]
toRomanNumeral x 
  | x == 0 = ""
  | otherwise = (snd . numToSubtract $ x) ++ toRomanNumeral(nextNum)
      where nextNum = x - (fst. numToSubtract $ x)

numToSubtract x = head (filter (lessThan x) characterMap)

lessThan n (a, b) | a <= n = True
                  | otherwise = False


-- toChange n returns the string of changes for the value n in dollars. 
-- For example, toChange 123.45 should return "1 - One Hundred\n2 - Ten\n3 - One\n1 - Quarter\n2 - Dime\n". 
-- Note that putStr (toChange 123.45) will look like the following:
-- 1 - One Hundred
-- 2 - Ten
-- 3 - One
-- 1 - Quarter
-- 2 - Dime
-- on the console screen.
toChange :: Float -> [Char]
toChange n = show (div num 10000) ++ " - One Hundred\n" ++ show (div (mod num 10000) 1000) ++ " - Ten\n" ++  show (div (mod num 1000) 100) ++ " - One\n" ++  show (div (mod num 100) 25) ++ " - Quarter\n" ++  show (div (mod num 25) 10) ++ " - Dime\n" 
              where num = round (n * 100)

list = [10000, 1000, 100, 25, 10]


-- void helper(double d, int n, double *coins) {
--  if(d == 0) return;
--  int i = d / coins[n];
--  printf("%d %f\n", i, coins[n]);
--  helper(d - (i * coins[n++]) , n, coins);
-- }


-- void toChange(string s) {
--  double d = stof(s);
--  double coins[5] = {100, 10, 1, 0.25, 0.1}; 
--  helper(d, 0, coins);
-- }


-- int main() {
--     toChange("111.1");
-- }