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
-- fromFloat Single 9.6
-- *Binary> fromFloat Single 0.1234
-- 00111101111111100101110010010010
-- *Binary> toFloat it
-- 0.1234

fromFloat :: Precision -> Float -> Binary
fromFloat percision n
    | n < 0 = FBinary Single ([One] ++ after)
    | n > 0 = FBinary Single ([Zero] ++ after)
    where after =  biasedExponentBinary n ++ intBinary ++ floatBinary 
          intBinary = if getIntPartLength n > 0 then (tail $ intPartInBinary n) else []
          floatBinary = if getIntPartLength n > 0 then (floatPartToBinary n (fitFloatLength n))
                        else fittedfloatPartToBinary n 0 False

biasedExponentBinary n
    | len < 8 = replicate (8 - len) Zero ++ result 
    | otherwise = result
    where result = charListToBitList $ intToBin (getExponent n + 127)
          len = length result

getExponent n 
    | getIntPartLength n <= 0 = negativeExponent (floatPartToBinary n (fitFloatLength n)) 0
    | getIntPartLength n > 0  = positiveExponent n

positiveExponent :: Float -> Int
positiveExponent n = (length $ intPartInBinary n) - 1

negativeExponent :: [Bit] -> Int -> Int
negativeExponent (x:xs) len -- len should be zero (for only decimal exists)
-- try to find 1 from left to right
    | x == Zero = negativeExponent xs (len+1)
    | x == One = negate (len+1)

getIntPart n
    | n >= 0 = floor n
    | n <  0 = abs $ ceiling n

getFloatPart n = abs n - fromIntegral(getIntPart n)

getFloatLength :: Float -> Int
getFloatLength n = length $ show $ getFloatPart n

getIntPartLength :: Float -> Int
getIntPartLength n = length $ intPartInBinary n 

fitFloatLength :: Float -> Int
fitFloatLength n 
    | intLength >  0 = 23 - intLength + 1 -- n is getIntPartLength
    | intLength == 0 = 23
    where intLength = getIntPartLength n

intPartInBinary :: Float -> [Bit] 
intPartInBinary n = charListToBitList $ intToBin $ getIntPart n

floatPartToBinary :: Float -> Int -> [Bit]
floatPartToBinary n floatLength -- float length: getFloatLength n
    | floatLength <= 0 = []
    | getIntPart((getFloatPart n) * 2) == 0 = [Zero] ++ nextIter
    | getIntPart((getFloatPart n) * 2) == 1 = [One] ++ nextIter
    where nextIter = floatPartToBinary (getFloatPart((getFloatPart n) * 2)) (floatLength - 1)

fittedfloatPartToBinary :: Float -> Int -> Bool -> [Bit]
fittedfloatPartToBinary n len metFirstOne
    | len >= 23 = []
    | getIntPart((getFloatPart n) * 2) == 0 && metFirstOne == True = [Zero] ++ nextIterOne
    | getIntPart((getFloatPart n) * 2) == 0 && metFirstOne == False = nextIter
    | getIntPart((getFloatPart n) * 2) == 1 = [One] ++ nextIterOne
    where nextIter = fittedfloatPartToBinary (getFloatPart((getFloatPart n) * 2)) (len) False 
          nextIterOne = fittedfloatPartToBinary (getFloatPart((getFloatPart n) * 2)) (len+1) True 


charListToInt :: [Char] -> Int
charListToInt x = read x :: Int

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
-- -- toFloat(FBinary Single n_)
-- n1 = charListToBitList "01000011001100100010000000000000" -- 178.125 v
-- n3 = charListToBitList "10111100001000111101011100001010" -- -0.01 v
-- n4 = charListToBitList "00111111100000010100011110101110" -- 1.01 v
-- n5 = charListToBitList "01000001001000000010100011110110" -- 10.01 v
-- n6 = charListToBitList "01000010110010000110010111100011" -- 100.199 v
-- n7 = charListToBitList "01000010110010001000000000000000" -- 100.25 v
-- n8 = charListToBitList "00111110000000000000000000000000" -- 0.125 v
-- n9 = charListToBitList "11000000010010010000111111011010" -- -3.1415925 v
-- n10 = charListToBitList "01111111110011001100110011001101" -- NAN
-- n11 = charListToBitList "01111111100000000000000000000000" -- inf
-- n12 = charListToBitList "11111111100000000000000000000000" -- -inf
-- n13 = charListToBitList "00111101100000000000000000000001"
-- n14 = charListToBitList "00111110000000000010100000000000" -- 0.125152587891
-- n15 = charListToBitList "00111100001000111101011100001010" -- 0.01 v
-- n2  = charListToBitList "00111100010100011110101110000101" 

takeExponent :: [Bit] -> Int
takeExponent n =  binToInt (binListToCharList (tail $ take 9 n)) - 127 -- exponent in int

takeMantissa :: [a] -> [a]
takeMantissa n =  drop 9 n

getIntPartFromMantissa :: [Bit] -> [Bit]
getIntPartFromMantissa n  
    | exponent == 128 = error "this is a NAN"
    | exponent > 0  = [One] ++ take exponent (takeMantissa n) 
    | exponent < 0  = [Zero]
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


