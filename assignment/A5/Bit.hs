-- Module Bit
-- manually make the Bit type an instance of the Show type class.

module Bit 
(Bit (Zero, One), bitValue, bitOr, bitAnd, bitNot) where 
    
data Bit = Zero | One deriving (Eq)

instance Show Bit where
    show Zero = "0"
    show One = "1"

-- This function turns a bit into a value, Zero is 0 and One is 1.
bitValue :: Num a => Bit -> a
bitValue Zero = 0
bitValue One = 1

bitOr :: Bit -> Bit -> Bit
bitOr Zero Zero = Zero
bitOr One _ = One
bitOr _ One = One

bitAnd :: Bit -> Bit -> Bit
bitAnd One One = One
bitAnd Zero _ = Zero
bitAnd _ Zero = Zero

bitNot :: Bit -> Bit
bitNot Zero = One
bitNot One = Zero