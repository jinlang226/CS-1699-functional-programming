module Binary 

(Binary (IBinary, FBinary), fromInt, toInt) where --, toFloat, fromFloat
import Bit

-- IBinary for integer and FBinary for floating-point
data Binary = IBinary Format BWidth [Bit] 
            | FBinary Precision [Bit]

-- only display a sequence of 0s and 1s on the screen
instance Show Binary where
    show (IBinary _ _ bits) = showBitList bits
    show (FBinary _ bits) = showBitList bits 

showBitList :: [Bit] -> String
showBitList [] =  ""
showBitList (x:xs) = show x ++ showBitList xs 

-- 8bit, 16-bit, and 32-bit
data BWidth = Byte | Half | Word deriving (Show, Eq)

data Format = Signed | Unsigned deriving (Show, Eq)

data Precision = Single | Double deriving (Show, Eq) 

fitLength original heading width = replicate (width - length(original)) heading ++ original  

getBWidth width | width == Byte = 8
                | width == Half = 16
                | width == Word = 32
                | otherwise = 0

-- This function returns a value of type Binary (IBinary) according to the given format, width, and an integer value.
fromInt :: Format -> BWidth -> Int -> Binary
fromInt format width n | format == Unsigned && n < 0 = error "error: n should not be less than 0 for Unsigned number"
                       | format == Unsigned  = IBinary format width fitted1
                       | format == Signed = IBinary format width fitted2
                       | otherwise = error "error"
                        where 
                            fitted1 = fitLength (charListToBitList(intToBin n)) Zero (getBWidth width)
                            fitted2 = fitLength (signedIntHelper(charListToBitList(intToBin (0-n)))) One (getBWidth width)

-- find the first 1, flip before
signedIntHelper :: [Bit] -> [Bit]
signedIntHelper n | last n == One && length(n) >  1 = flipBits (init n) ++ [One]
                  | last n == One && length(n) == 1 = [One]
                  | last n == Zero = signedIntHelper (init n) ++ [Zero]
                  | otherwise = error "error"
                 
flipBits [One]  = [Zero]
flipBits [Zero] = [One]
flipBits (x:xs) | last (x:xs) == Zero = (flipBits (init (x:xs))) ++ [One]
                | last (x:xs) == One = (flipBits (init (x:xs))) ++ [Zero]


-- intToBin n returns the unsigned binary representation (as a string of 0s and 1s) of the given positive number n. For example, intToBin 22 is "10110". You can assume that the given integer will be greater than or equal to 0.
intToBin :: Int -> [Char]
intToBin 0 = []
intToBin 1 = show 1
intToBin n =  (intToBin (n `div` 2)) ++ show (n `mod` 2)

charListToBitList :: [Char] -> [Bit]
charListToBitList [] = []
charListToBitList (x:xs) | last (x:xs) == '0' = (charListToBitList (init (x:xs))) ++ [Zero]
                         | last (x:xs) == '1' = (charListToBitList (init (x:xs))) ++ [One]
                         | otherwise = error "not possible"


-- This function turns a value of type Binary (IBinary) to an integer
toInt :: Binary -> Int
toInt (IBinary format width bits) 
    | format == Unsigned = binToInt (binListToCharList bits)
    | format == Signed   = binToInt $ binListToCharList $ flipBits $ subOneBinary bits

subOneBinary :: [Bit] -> [Bit]
subOneBinary [] = []
subOneBinary bits | last bits == One  = init bits ++ [Zero]
                  | last bits == Zero = subOneBinary (init bits) ++ [One] 


binListToCharList :: [Bit] -> [Char]
binListToCharList [] = []
binListToCharList (x:xs) | last (x:xs) == Zero = (binListToCharList (init (x:xs))) ++ "0"
                         | last (x:xs) == One = (binListToCharList (init (x:xs))) ++ "1"
                         | otherwise = error "not possible"


binToInt :: [Char] -> Int
binToInt [] = 0
binToInt (x:xs) = (scanChar x * 2^(length xs)) + (binToInt xs)

scanChar c 
    | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'




-- This function returns a value of type Binary (FBinary) according to the given precision and a floating-point value. You only need to support single precision. No worry about double precision.
-- 1, 8 exponent, 23 mantissa
fromFloat :: Precision -> Float -> Binary
fromFloat percision n
    | n < 0 = FBinary Single [One] 
    | n > 0 = FBinary Single [Zero] 

n :: Float
n = 9.8

getIntPart :: Float -> [Char] 
getIntPart n = take (length (show $(floor n))) (show n)

getFloatPart :: Float -> [Char]
getFloatPart n = drop (length (show $(floor n)) + 1) (show n)

getFloatLength :: Float -> Int
getFloatLength n = (length $ show n) - (length (show $(floor n))) - 1

getIntPartLength :: Float -> Int
getIntPartLength n = length(getIntPart n)

fitIntPart :: [Char] -> Int -> ([Char], Int)
fitIntPart (x:xs) len
    | x == '0' = fitIntPart xs (len - 1)
    | x == '1' = ((x:xs), len)

fitFloatLength n = 23 - snd (fitIntPart n (length n)) -- n is getIntPart

-- floatPartToBinary :: Int -> Int -> [Char]
-- floatPartToBinary n floatLength
--     | floatLength <= 0 = []       
--     | length(show (n*2)) <= (length $ show n) && head(show(n*2)) == '1' = "1_1, " ++ (floatPartToBinary (charListToInt(tail (show (n*2)))) (floatLength-1)) -- 1
--     | length(show (n*2)) <= (length $ show n) && head(show(n*2)) /= '1' = "0_2, " ++ floatPartToBinary (n*2) (floatLength-1) -- 0
--     | (restAreZero n)  && length(show (n*2)) >  (length $ show n) && head(show(n*2)) == '1' = "1_3, "
--     | not (restAreZero n)  && length(show (n*2)) >  (length $ show n) && head(show(n*2)) == '1' = "1_4, " ++ (floatPartToBinary (charListToInt(tail (show (n*2)))) (floatLength-1)) -- 1
--     | length(show (n*2)) >  (length $ show n) && head(show(n*2)) /= '1' = "error"

floatPartToBinary :: [Char] -> Int -> [Char]
floatPartToBinary numChar floatLength
    | floatLength <= 0 = []       
    | length(show (n*2)) <= (length $ show n) && head(show(n*2)) == '1' = "1_1, " ++ (floatPartToBinary (charListToInt(tail (show (n*2)))) (floatLength-1)) -- 1
    | length(show (n*2)) <= (length $ show n) && head(show(n*2)) /= '1' = "0_2, " ++ floatPartToBinary (n*2) (floatLength-1) -- 0
    | (restAreZero n)  && length(show (n*2)) >  (length $ show n) && head(show(n*2)) == '1' = "1_3, "
    | not (restAreZero n)  && length(show (n*2)) >  (length $ show n) && head(show(n*2)) == '1' = "1_4, " ++ (floatPartToBinary (charListToInt(tail (show (n*2)))) (floatLength-1)) -- 1
    | length(show (n*2)) >  (length $ show n) && head(show(n*2)) /= '1' = "error"
    where n = charListToInt numChar

charListToInt :: [Char] -> Int
charListToInt x = read x :: Int

restAreZero :: Int -> Bool
restAreZero n 
    | n `mod` 10 == 0 = True
    | otherwise = False

charListToIntList :: [Char] -> [Int]
charListToIntList [] = []
charListToIntList (x:xs) = [scanChar x] ++ (charListToIntList xs) 


-- This function turns a value of type Binary (Fbinary) to a floating-point number. Again, only focus on single precision.
toFloat :: Binary -> Float
toFloat (FBinary percision bits)
    | bits == n11 = error "this is INF"
    | bits == n12 = error "this is -INF"
    | head bits == One = negate (sum(twoExponentFst(binListToCharList(getIntPartFromMantissa bits))) + sum(twoExponentSnd(binListToCharList(getDecimalPartFromMantissa bits))) ) -- negative, integer+float
    | head bits == Zero = sum(twoExponentFst(binListToCharList(getIntPartFromMantissa bits))) + sum(twoExponentSnd(binListToCharList(getDecimalPartFromMantissa bits))) -- positive, integer+float
    | otherwise = error "error"

-- test
n1 = charListToBitList "01000011001100100010000000000000" -- 178.125 v
n2 = charListToBitList "00111111100000010100011110101110" -- 0.01 v
n3 = charListToBitList "10111100001000111101011100001010" -- -0.01 v
n4 = charListToBitList "00111111100000010100011110101110" -- 1.01 v
n5 = charListToBitList "01000001001000000010100011110110" -- 10.01 v
n6 = charListToBitList "01000010110010000110010111100011" -- 100.199 v
n7 = charListToBitList "01000010110010001000000000000000" -- 100.25 v
n8 = charListToBitList "00111110000000000000000000000000" -- 0.125 v
n9 = charListToBitList "11000000010010010000111111011010" -- -3.1415925 v
n10 = charListToBitList "01111111110011001100110011001101" -- NAN
n11 = charListToBitList "01111111100000000000000000000000" -- inf
n12 = charListToBitList "11111111100000000000000000000000" -- -inf

takeExponent :: [Bit] -> Int
takeExponent n =  binToInt (binListToCharList (tail $ take 9 n)) - 127 -- exponent in int

takeMantissa :: [a] -> [a]
takeMantissa n =  drop 9 n

getIntPartFromMantissa :: [Bit] -> [Bit]
getIntPartFromMantissa n 
    | exponent == 128 = error "this is a NAN"
    | exponent > 0  = [One] ++ take exponent (takeMantissa n) 
    | exponent < 0  = [One]
    | exponent == 0 = [One]
    where exponent = takeExponent n

normalisedPart :: [Bit] -> [Bit]
normalisedPart [] = [Zero]
normalisedPart n = [One] ++ n 

getDecimalPartFromMantissa :: [Bit] -> [Bit]
getDecimalPartFromMantissa n 
    | exponent >= 0 = drop exponent (takeMantissa n) 
    | exponent < 0  = if head mantissa == One then replic ++ mantissa else replic ++ [One] ++ mantissa 
    where exponent = takeExponent n
          mantissa = takeMantissa n
          replic = replicate ((negate exponent)-1) Zero

-- binary abc.de = a * 2^(2) + b * 2^(1) + c * 2^(0) + d * 2^(-1) + e * 2^(-2)
twoExponentSnd :: [Char] -> [Float]
twoExponentSnd n = zipWith (*) (map recip (zipWith (^) (replicate (length n) 2) [1..(length n)])) (toIntList n)

twoExponentFst :: [Char] -> [Float]
twoExponentFst n = zipWith (*) ((zipWith (^) (replicate (length n) 2)) (reverse [0..((length n) -1)])) (toIntList n) 

toIntList :: [Char] -> [Float]
toIntList (x:xs) = [fromIntegral (scanChar x)] ++ toIntList xs

